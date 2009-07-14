
(** Generate package files
    @author Sylvain Le Gall
  *)

open Format;;
open OASISTypes;;
open BaseUtils;;
open BaseFileGenerate;;
open BaseGenCode;;

(** Type for OCaml module embedded code
  *)
type modul = string;;

(** Standard variables 
  *)
type standard_var = 
  | SVocamlc
  | SVocamlopt
  | SVocamlbest
  | SVsuffix_program
  | SVocaml_version
  | SVstandard_library_default
  | SVstandard_library
  | SVstandard_runtime
  | SVccomp_type
  | SVbytecomp_ccompiler
  | SVbytecomp_c_linker
  | SVbytecomp_c_libraries
  | SVnative_c_compiler
  | SVnative_c_linker
  | SVnative_c_libraries
  | SVnative_partial_linker
  | SVranlib
  | SVcc_profile
  | SVarchitecture
  | SVmodel
  | SVsystem
  | SVext_obj
  | SVext_asm
  | SVext_lib
  | SVext_dll
  | SVos_type
  | SVdefault_executable_name
  | SVsysthread_supported
;;

(** Describe action made by a target
  *)
type generator_action =
    { 
      (** OCaml module to be added to setup.ml *)
      moduls: modul list;

      (** Function to be added to BaseSetup.t *)
      setup_code: BaseGenCode.ocaml_expr;

      (** Function to be called when cleaning *)
      clean_code: BaseGenCode.ocaml_stmts;

      (** Function to be called when distcleaning *)
      distclean_code: BaseGenCode.ocaml_stmts;

      (** Write extra files *)
      other_action: unit -> unit; 

      (** Files generated *)
      files_generated: filename list;

      (** Standard variable used *)
      standard_vars: standard_var list;
    }
;;

(** Kind of targets 
  *)
type generator_kind =
  | Build
  | Doc
  | Test
  | Install
;;

module MapGenerator = Map.Make (
struct 
  type t = generator_kind * string 

  let compare (knd1, nm1) (knd2, nm2) = 
    match compare knd1 knd2 with 
      | 0 ->
          String.compare 
            (String.lowercase nm1)
            (String.lowercase nm2)
      | n ->
          n
end)
;; 

let allkind_generators =
  ref MapGenerator.empty
;;

(** Register a new generator *)
let generator_register knd nm fact =
  allkind_generators := MapGenerator.add (knd, nm) fact !allkind_generators
;;

module MapString = Map.Make(String)
;;

let configure_generators =
  ref MapString.empty
;;

(** Register a new configure generator 
  *)
let configure_generator_register nm act =
  configure_generators := MapString.add nm act !configure_generators
;;

(** Convert target_kind to string 
  *)
let string_of_generator_kind =
  function
    | Build   -> "build"
    | Doc     -> "doc"
    | Test    -> "test"
    | Install -> "install"
;;

(** Generate autobuild system 
  *)
let generate pkg = 

  let get_generator knd fnm pkg =
    let nm =
      fnm pkg
    in
      try
        let fact =
          MapGenerator.find (knd, nm) !allkind_generators 
        in
          fact pkg
      with Not_found ->
        (
          let availables =
            MapGenerator.fold
              (fun (knd2, nm) _ acc ->
                 if knd2 = knd then
                   nm :: acc
                 else
                   acc)
              !allkind_generators
              []
          in
            failwith 
              (Printf.sprintf 
                 "Unkown %s scheme '%s' (available: %s)"
                 (string_of_generator_kind knd)
                 nm
                 (String.concat ", " availables))
        )
  in

  let generators, pkg =
    List.fold_left
      (fun (generators, pkg) (setup_nm, knd, fnm) ->
         let act, pkg =
           get_generator knd fnm pkg
         in
           ((setup_nm, act) :: generators), pkg)
      ([], pkg)
      [
        "build",     Build,   (fun pkg -> pkg.build_type);
        "doc",       Doc,     (fun pkg -> pkg.doc_type);
        "test",      Test,    (fun pkg -> pkg.test_type);
        "install",   Install, (fun pkg -> pkg.install_type);
      ]
  in

  let standard_vars =
    let module SVSet = 
      Set.Make
        (struct 
           type t = standard_var 
           let compare = compare
         end)
    in
      SVSet.elements
        (List.fold_left
           (fun set e -> SVSet.add e set)
           SVSet.empty
           (List.flatten 
              (List.map 
                 (fun (_, act) -> act.standard_vars)
                 generators)))
  in

  let configure =
    try
      let fact =
        MapString.find pkg.conf_type !configure_generators
      in
        fact pkg standard_vars
    with Not_found ->
      (
        let availables =
          MapString.fold
            (fun nm _ acc -> nm :: acc)
            !configure_generators
            []
        in
          failwith
            (Printf.sprintf
               "Unknow configure scheme '%s' (available: %s)"
               pkg.conf_type
               (String.concat ", " availables))
      )
  in

  let all_actions =
    configure
    ::
    (List.map snd generators)
  in

  let clean_code =
    FUN 
      (["()"],
       (List.flatten (List.map (fun act -> act.clean_code) all_actions)))
  in

  let distclean_code =
    FUN 
      (["()"],
       (List.flatten (List.map (fun act -> act.distclean_code) all_actions)))
  in

  let files_generated_code =
    LST
      (List.map 
         (fun f -> STR f)
         (List.flatten (List.map (fun act -> act.files_generated) all_actions)))
  in

  let setup_t_code =
    REC
      ("BaseSetup",
       (
         (List.map (fun (nm, act) -> nm, act.setup_code) generators)
         @
         [
           "configure",       configure.setup_code;
           "clean",           clean_code;
           "distclean",       distclean_code;
           "files_generated", files_generated_code;
         ]
       )
      )
  in

  let setup_fun =
    fprintf str_formatter
      "@[<hv2>let setup () =@ %a@,@];;"
      pp_ocaml_expr (APP ("BaseSetup.setup", [setup_t_code]));
    flush_str_formatter ()
  in

  let moduls =
    let module SSet = Set.Make(String)
    in
    let moduls = 
      List.flatten
        (List.map (fun act -> act.moduls) all_actions)
    in
    let (rmoduls, _) =
      List.fold_left
        (fun ((moduls, moduls_seen) as acc) modul ->
           if SSet.mem modul moduls_seen then
             acc
           else
             (modul :: moduls, SSet.add modul moduls_seen))
        ([], SSet.empty)
        moduls
    in
      List.rev rmoduls
  in

    (* Generate setup.ml *)
    mlfile_generate
      "setup.ml"
      (Split
         (
           (* Header *)
           [],
           (* Body *)
           (List.flatten 
              [
                moduls;
                [setup_fun]
              ]),
           (* Footer *)
           ["setup ();;"]
         )
      );

    (* Generate Makefile (for standard dev. env.) *)
    file_generate
      "Makefile"
      comment_sh
      (NeedSplit
         BaseData.makefile);

    (* Generate configure (for standard dev. env.) *)
    file_generate
      "configure"
      comment_sh
      (NeedSplit
         BaseData.configure);
    Unix.chmod "configure" 0o755;

    (* Generate other files *)
    List.iter
      (fun act -> act.other_action ())
      all_actions
;;

