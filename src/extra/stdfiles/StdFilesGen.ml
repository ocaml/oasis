
(** Generate standard text files: INSTALL.txt, README.txt
    @author Sylvain Le Gall
  *)

open BaseFileGenerate
open OASISPlugin
open OASISTypes
open OASISUtils
open OASISValues
open OASISGettext
open Format
open FormatExt

module PU = OASISPlugin.Extra.Make
              (struct
                 let name    = "StdFiles"
                 let version = OASISConf.version
               end)
open PU

type package =
    (* Standalone executable *)
  | LTool of filename
    (* Findlib package *)  
  | LFindlibPackage of name * (version_comparator option)

let facts = 
  [
    LFindlibPackage ("ocaml", None),
    [
      (* Tools shipped with OCaml *)
      "camlp4";
      "camlp4boot";
      "camlp4o";
      "camlp4of";
      "camlp4of.opt";
      "camlp4oof";
      "camlp4oof.opt";
      "camlp4o.opt";
      "camlp4orf";
      "camlp4orf.opt";
      "camlp4prof";
      "camlp4r";
      "camlp4rf";
      "camlp4rf.opt";
      "camlp4r.opt";
      "mkcamlp4";
      "ocaml";
      "ocamlbuild";
      "ocamlbuild.byte";
      "ocamlbuild.native";
      "ocamlc";
      "ocamlc.opt";
      "ocamlcp";
      "ocamldebug";
      "ocamldep";
      "ocamldep.opt";
      "ocamldoc";
      "ocamldoc.opt";
      "ocamllex";
      "ocamllex.opt";
      "ocamlmklib";
      "ocamlmktop";
      "ocamlopt";
      "ocamlopt.opt";
      "ocamlprof";
      "ocamlrun";
      "ocamlyacc";
    ],
    [
      (* Libraries shipped with OCaml *)
      "bigarray";
      "camlp4";
      "dbm";
      "dynlink";
      "graphics";
      "labltk";
      "num";
      "stdlib";
      "str";
      "threads";
      "unix";
    ];

    LFindlibPackage ("findlib", None), 
    ["ocamlfind"],
    ["findlib"];
  ]

(** Merge 2 versions constraint *)
let merge_version_opt ver_opt1 ver_opt2 =
 match ver_opt1, ver_opt2 with
   | Some v1, Some v2 -> Some (VAnd (v1, v2))
   | None, v | v, None -> v

(** Associate a tool to a findlib package or a tool *)
let package_of_tool = 
  let mp = 
    map_string_of_assoc
      (List.fold_left
         (fun acc (pkg, tools, _) ->
            List.fold_left
              (fun acc tool -> (tool, pkg) :: acc)
              acc
              tools)
         []
         facts)
  in
    fun tool ->
      try 
        MapString.find tool mp
      with Not_found ->
        LTool tool

(** Associate a library to a findlib package raw data *)
let package_of_library = 
  let mp = 
    map_string_of_assoc
      (List.fold_left
         (fun acc (pkg, _, libs) ->
            List.fold_left
              (fun acc lib -> (lib, pkg) :: acc)
              acc
              libs)
         []
         facts)
  in
    fun lib ver_opt ->
      try 
        MapString.find lib mp
      with Not_found ->
        LFindlibPackage (lib, ver_opt)

let fn_enable fn = 
  new_field 
    OASISPackage.schema
    fn 
    ~default:true 
    boolean
    (fun () ->
       Printf.sprintf (f_ "Enable %s file generation.") fn),
  new_field 
    OASISPackage.schema
    (fn^"Filename")
    ~default:(fn^".txt")
    string_not_empty
    (fun () -> 
       Printf.sprintf (f_ "Real filename to use for file %s.") fn)

let readme =
  fn_enable "README"

let install = 
  fn_enable "INSTALL"

let authors =
  fn_enable "AUTHORS"

module SetSection = 
  Set.Make
    (struct type t = section let compare = compare end)

let main pkg = 
  let data =
    pkg.schema_data
  in

  let pp_print_cut2 fmt () =
    pp_print_cut fmt ();
    pp_print_cut fmt ()
  in

  let pp_print_para fmt str = 
    pp_open_vbox fmt 0;
    pp_open_box fmt 0;
    String.iter 
      (function
         | ' ' -> pp_print_space fmt ()
         | '\n' -> 
             pp_close_box fmt ();
             pp_print_cut2 fmt ();
             pp_open_box fmt 0;
         | c -> pp_print_char fmt c)
      str;
    pp_close_box fmt ();
    pp_print_cut2 fmt ();
    pp_close_box fmt ()
  in

  let pp_print_title fmt str =
    (* TODO: use Markdown formatting *)
    fprintf fmt "@[%s@]@," str;
    fprintf fmt "@[%s@]@,@," (String.make (String.length str) '=')
  in

  (** All sections that contains a build_section *)
  let all_build_sections_set = 
    let all_build_sections =
      List.rev
        (List.fold_left
           (fun acc ->
              function
                | Library (_, bs, _) 
                | Executable (_, bs, _) as sct ->
                    (sct, bs) :: acc
                | SrcRepo _ | Flag _ | Test _ ->
                    acc)
           []
           pkg.sections)
    in
      List.fold_left
        (fun acc e -> SetSection.add e acc)
        SetSection.empty  
        (List.rev_map fst all_build_sections)
  in

  let pp_print_sections =
    (* If a tool/library applies to all sections that can handle a build_depends
       or build_tools we can say it is a general depends. In this case, return
       is None, otherwise "Some sections".
     *)
    fun fmt sections ->
      let is_all_build_sections, sections = 
        if SetSection.subset all_build_sections_set sections then
          true, SetSection.diff sections all_build_sections_set
        else
          false, sections
      in
        if SetSection.is_empty sections then
          ()
        else
          fprintf fmt " for %t%a"
            (fun fmt ->
               if is_all_build_sections then
                 fprintf fmt "all,@ ")
            (pp_print_list 
               (fun fmt ->
                  function
                    | Library ({cs_name = nm}, _, _) -> 
                        fprintf fmt "library %s" nm
                    | Executable ({cs_name = nm}, _, _) -> 
                        fprintf fmt "executable %s" nm
                    | Test ({cs_name = nm}, _) ->
                        fprintf fmt "test %s" nm
                    | SrcRepo _ | Flag _ ->
                        ())
               ",@ ")
            (SetSection.elements sections)
  in

  let pp_print_ver_opt fmt =
    function
      | Some ver_cmp ->
          fprintf fmt " (%a)"
            pp_print_string 
            (OASISVersion.string_of_comparator 
               (OASISVersion.comparator_reduce ver_cmp))
      | None ->
          ()
  in

  let depends = 
    let merge_package_section lst (new_pkg, new_sections) = 
      try
        let old_pkg, new_pkg, old_sections = 
          match new_pkg with 
            | LTool _ as tool ->
                tool, 
                tool,
                List.assoc tool lst
                          
            | LFindlibPackage (nm1, ver_opt1) ->
                begin
                  let res_opt = 
                    List.fold_left
                      (fun acc pkg ->
                         match acc, pkg with 
                           | None, 
                             ((LFindlibPackage (nm2, ver_opt2)) 
                                as old_pkg, 
                              sections) -> 
                               if nm1 = nm2 then
                                 Some 
                                   (old_pkg,
                                    LFindlibPackage 
                                      (nm1, 
                                       merge_version_opt 
                                         ver_opt1 
                                         ver_opt2),
                                    sections)
                               else 
                                 acc
                           | _, _ ->
                               acc)
                      None
                      lst
                  in
                    match res_opt with
                      | Some res ->
                          res
                      | None ->
                          raise Not_found
                end
        in
        let lst = 
          try 
            List.remove_assoc old_pkg lst
          with Not_found ->
            lst
        in
          (new_pkg, SetSection.union new_sections old_sections) :: lst

      with Not_found ->
        (new_pkg, new_sections) :: lst
    in

    let add_build_tools ssection = 
       List.fold_left
         (fun lst ->
            function
              | ExternalTool tool ->
                  (package_of_tool tool, ssection) :: lst
              | InternalExecutable _ ->
                  lst)
    in

    let split_package_section = 
      List.fold_left 
        (fun lst ->
           function 
             | Library (_, bs, _) | Executable (_, bs, _) as section ->
                 begin
                   let ssection = 
                     SetSection.singleton section
                   in
                   let lst = 
                     (* Add build_depends *)
                     List.fold_left
                       (fun lst ->
                          function
                            | FindlibPackage (fndlb_nm, ver_opt) ->
                                let fndlb_root = 
                                  match (split '.' fndlb_nm) with 
                                    | hd :: _ -> hd
                                    | _ -> fndlb_nm
                                in
                                  (package_of_library fndlb_root ver_opt,
                                   ssection) :: lst 
                            | InternalLibrary _ ->
                                lst)
                       lst
                       bs.bs_build_depends
                   in
                     (* Add build_tools *)
                     add_build_tools 
                       ssection
                       lst
                       bs.bs_build_tools
                 end
             | Test (_, {test_build_tools = build_tools}) as section ->
                 add_build_tools
                   (SetSection.singleton section)
                   lst
                   build_tools
             | Flag _ | SrcRepo _ ->
                 lst) 

        (* Basic dependencies *)
        [
          LFindlibPackage ("findlib", pkg.findlib_version), all_build_sections_set;
          LFindlibPackage ("ocaml", pkg.ocaml_version), all_build_sections_set;
        ]

        (* Go through all sections *)
        pkg.sections
    in

      (* Merge everything *)
      List.fold_left
        merge_package_section
        []
        split_package_section
  in

  let file_generate (enable, fn) ppf = 
    if enable data then
      (
        let content = 
          ppf str_formatter;
          flush_str_formatter ()
        in
          file_generate 
            (fn data)
            comment_ml
            (Split ([], [content], []))
      )
  in

  let install_enable, install_fn =
    install
  in

    (* Generate README.txt *)
    file_generate readme
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt 
           "@[This is the README file for the %s distribution.@]@,@,"
           pkg.name;

         List.iter
           (pp_print_para fmt)
           pkg.copyrights;
         
         pp_print_para fmt pkg.synopsis;
         
         begin
           match pkg.description with 
             | Some str ->
                 pp_print_para fmt str
             | None ->
                 ()
         end;

         pp_open_box fmt 0;
         if install_enable data then
           begin
             fprintf fmt
               "See@ the@ files@ %s@ for@ building@ and@ installation@ instructions.@ "
               (install_fn data)
           end;
         begin
           match pkg.license_file with 
             | Some fn ->
                 fprintf fmt "See@ the@ file@ %s@ for@ copying@ conditions.@ " fn
             | None ->
                 ()
         end;
         pp_close_box fmt ();
         pp_print_cut2 fmt ();

         begin
           match pkg.homepage with 
             | Some url ->
                 fprintf fmt
                   "@[<hv2>Home page: %s@]@,@," url;
             | None -> 
                 ()
         end;

         pp_close_box fmt ());

    (* Generate INSTALL.txt *)
    file_generate install
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt 
           "@[This is the INSTALL file for the %s distribution.@]@,@,"
           pkg.name;

         pp_print_para fmt
           "This package uses OASIS to generate its build system. \
            See section OASIS for full information. ";

         pp_print_title fmt "Dependencies";
         fprintf fmt "@[In order to compile this package, you will need:@]@,";
         pp_open_vbox fmt 0;

         pp_print_list 
           (fun fmt (pkg, sections) ->
              fprintf fmt "* @[%a%a@]" 
                (fun fmt ->
                   function
                     | LTool s -> 
                         pp_print_string fmt s
                     | LFindlibPackage (nm, ver_opt) ->
                         fprintf fmt "%s%a"
                           nm
                           pp_print_ver_opt ver_opt)
                pkg
                pp_print_sections sections)
           "@,"
           fmt
           depends;

         pp_close_box fmt ();
         pp_print_cut fmt ();

         pp_print_string fmt StdFilesGenData.install;
         pp_close_box fmt ());
    
    (* Generate AUTHORS.txt *)
    file_generate authors
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt "@[Authors of %s@]@," pkg.name;

         pp_print_list
           pp_print_string
           "@,"
           fmt
           pkg.authors;

         if pkg.maintainers <> [] then
           begin
             pp_print_cut2 fmt ();
             fprintf fmt "@[Current maintainers of %s@]@," pkg.name;
             pp_print_list
               pp_print_string
               "@,"
               fmt
               pkg.maintainers
           end;

         pp_close_box fmt ())

let () = 
  register main
