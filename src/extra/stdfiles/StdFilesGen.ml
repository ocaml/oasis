
(** Generate standard text files: INSTALL.txt, README.txt
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASIS
open OASISUtils
open OASISValues
open BasePlugin
open BaseFileGenerate
open CommonGettext
open Format
open FormatExt


let plugin_id = "StdFiles"

let fn_enable fn = 
  new_field 
    OASISPackage.schema 
    plugin_id 
    fn 
    ~default:true 
    boolean
    (fun () ->
       Printf.sprintf (f_ "Enable %s file generation.") fn),
  new_field 
    OASISPackage.schema
    plugin_id 
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

type section =
  | Executable of name * executable
  | Library of name * library

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

  let all_build_sections =
     List.rev_append
       (List.rev_map 
          (fun (nm, lib) -> 
             Library (nm, lib), 
             lib.lib_build_depends, 
             lib.lib_build_tools)
          pkg.libraries)
       (List.map 
          (fun (nm, exec) -> 
             Executable (nm, exec), 
             exec.exec_build_depends,
             exec.exec_build_tools)
          pkg.executables)
  in

  let pp_print_sections =
    (* If a tool/library applies to all sections that can handle a build_depends
       or build_toolsn we can say it is a general depends. In this case, return
       is None, otherwise "Some sections".
     *)
    let ref_general = 
      List.fold_left
        (fun acc e -> SetSection.add e acc)
        SetSection.empty  
        (List.rev_map 
           (fun (section, _, _) -> section)
           all_build_sections)
    in
      fun fmt sections ->
        if SetSection.equal sections ref_general then
          ()
        else
          fprintf fmt " (%a)"
            (pp_print_list 
               (fun fmt ->
                  function
                    | Library (nm, _) -> 
                        fprintf fmt "library %s" nm
                    | Executable (nm, _) -> 
                        fprintf fmt "executable %s" nm)
               ",@ ")
            (SetSection.elements sections)
  in

  let pp_print_ver_opt fmt =
    function
      | Some ver_cmp ->
          fprintf fmt " %a"
            pp_print_string 
            (OASISVersion.string_of_comparator 
               (OASISVersion.comparator_reduce ver_cmp))
      | None ->
          ()
  in


  let build_depends =
    let map_build_depends = 
      List.fold_left
        (fun acc (section, build_depends, _) ->
           List.fold_left
             (fun acc ->
                function
                  | FindlibPackage (fndlb_nm, ver_opt1) ->
                     let ver_opt2, st =
                        try
                          MapString.find fndlb_nm acc
                        with Not_found ->
                          None, SetSection.empty
                      in
                      let ver_opt =
                        match ver_opt1, ver_opt2 with
                          | Some v1, Some v2 -> Some (VAnd (v1, v2))
                          | None, v | v, None -> v
                      in
                        MapString.add 
                          fndlb_nm 
                          (ver_opt, 
                           SetSection.add section st)
                          acc
                     
                  | InternalLibrary _ ->
                      acc)
             acc
             build_depends)
        MapString.empty
        all_build_sections
    in
      MapString.fold
        (fun fndlb_nm (ver_opt, sections) acc ->
           (fndlb_nm, ver_opt, sections) :: acc)
        map_build_depends
        []
  in

  let build_tools = 
    let map_build_tools =
      List.fold_left
        (fun mp (section, _, build_tools) ->
           List.fold_left
             (fun acc tool ->
                (* TODO: make a difference between internal 
                   and external tools
                 *)
                 let st =
                   try
                     MapString.find tool mp
                   with Not_found ->
                     SetSection.empty
                 in
                   MapString.add 
                     tool
                     (SetSection.add section st)
                     mp)
             mp
             build_tools)
        MapString.empty
        all_build_sections
    in
      MapString.fold
        (fun fndlb_nm sections acc ->
           (fndlb_nm, sections) :: acc)
        map_build_tools
        []
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
           "This package use ocaml-autobuild to generate its build system. \
            See section ocaml-autobuild for full information. ";

         pp_print_title fmt "Dependencies";
         fprintf fmt "@[In order to compile this package, you will need:@]@,";
         pp_open_vbox fmt 0;
         pp_print_list 
           (fun fmt (tool, sections) ->
              fprintf fmt "* @[%s%a@]" 
                tool 
                pp_print_sections sections)
           "+@,"
           fmt
           build_tools;

         if build_tools <> [] && build_depends <> [] then
           pp_print_cut fmt ();

         pp_print_list 
           (fun fmt (tool, ver_opt, sections) ->
              fprintf fmt "* @[findlib library %s%a%a@]" 
                tool 
                pp_print_ver_opt ver_opt
                pp_print_sections sections)
           "@,"
           fmt
           build_depends;
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
  plugin_register 
    plugin_id 
    (Extra main)
