
(** Generate package files
    @author Sylvain Le Gall
  *)

open Format;;
open OASISTypes;;
open BaseUtils;;
open BaseFileGenerate;;

(* TODO: replace this by only pre_pkg *)
type target_data =
    {
      (* OASIS package as interpreted at generation time *)
      pre_pkg: OASISTypes.package;
    }
;;

(** Type for OCaml module embedded code
  *)
type modul = string;;

(** Describe action made by a target
  *)
type generator_action =
    { 
      (** OCaml module to be added to setup.ml *)
      moduls: modul list;

      (** Function to be added to BaseSetup.t *)
      pp_setup_fun: formatter -> unit -> unit;

      (** Function to be called when cleaning *)
      pp_clean_fun: (formatter -> unit -> unit) option;

      (** Function to be called when distcleaning *)
      pp_distclean_fun: (formatter -> unit -> unit) option;

      (** Write extra files *)
      other_action: unit -> unit; 
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
let generate data = 

  let get_generator knd fnm data =
    let nm =
      fnm data
    in
      try
        let fact =
          MapGenerator.find (knd, nm) !allkind_generators 
        in
          fact data
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

  let generators, data =
    List.fold_left
      (fun (generators, data) (setup_nm, knd, fnm) ->
         let act, data =
           get_generator knd fnm data
         in
           ((setup_nm, act) :: generators), data)
      ([], data)
      [
        "build",     Build,   (fun data -> data.pre_pkg.build_type);
        "doc",       Doc,     (fun data -> data.pre_pkg.doc_type);
        "test",      Test,    (fun data -> data.pre_pkg.test_type);
        "install",   Install, (fun data -> data.pre_pkg.install_type);
      ]
  in

  let configure =
    try
      let fact =
        MapString.find data.pre_pkg.conf_type !configure_generators
      in
        fact data
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
               data.pre_pkg.conf_type
               (String.concat ", " availables))
      )
  in

  let all_actions =
    configure
    ::
    (List.map snd generators)
  in

  let pp_parent_protect pp_elem fmt elem =
     fprintf fmt "@[(@[@,%a@]@,)@]" pp_elem elem
  in

  let pp_setup_field ?(parent=true) nm pp_elem fmt elem =
    pp_record_field 
      fmt 
      ("BaseSetup."^nm)
      (if parent then
         pp_parent_protect pp_elem
       else
         pp_elem) 
      elem;
    pp_record_sep fmt ();
  in

  let pp_commonclean get_pp_fun fmt () = 
    let () = 
      fprintf fmt "fun _ ->@, "
    in
    let output_smthg =
      List.fold_left 
        (fun output_smthg act ->
           let pp_fun =
             get_pp_fun act
           in
             match pp_fun with 
               | Some pp_fun -> 
                   pp_parent_protect pp_fun fmt ();
                   fprintf fmt ";@, ";
                   true
               | _ ->
                   output_smthg)
        false
        all_actions
    in
      if not output_smthg then
        pp_print_string fmt "()"
  in

  let pp_clean =
    pp_commonclean (fun act -> act.pp_clean_fun) 
  in

  let pp_distclean =
    pp_commonclean (fun act -> act.pp_distclean_fun)
  in

  let pp_setup_t fmt () =
      pp_record_open fmt ();
      List.iter
        (fun (nm, act) -> pp_setup_field nm act.pp_setup_fun fmt ())
        generators;
      pp_setup_field "configure" configure.pp_setup_fun fmt ();
      pp_setup_field "clean" pp_clean fmt ();
      pp_setup_field "distclean" pp_distclean fmt ();
      pp_record_close fmt ()
  in

  let setup_fun =
    fprintf str_formatter
      "@[<hv>let setup () =@, @[<hv>BaseSetup.setup@, %a@]@,@];;"
      pp_setup_t ();
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
                [
                  "";
                  "#use \"topfind\";;";
                  "#require \"findlib\";;";
                  "#require \"fileutils\";;";
                  "";
                ];
                moduls;
                [
                  setup_fun;
                ]
              ]),
           (* Footer *)
           [""; "setup ();;"]
         )
      );

    (* Generate Makefile (for standard dev. env.) *)
    file_generate
      "Makefile"
      comment_sh
      (NeedSplit
         BaseData.makefile);

    (* Generate other files *)
    List.iter
      (fun act -> act.other_action ())
      all_actions
;;

