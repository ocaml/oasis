
(** Generate standard text files: INSTALL.txt, README.txt
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASIS;;
open OASISValueParser;;
open BasePlugin;;
open BaseFileGenerate;;
open CommonGettext;;

let plugin_id = "StdFiles";;

let fn_enable fn = 
  new_field 
    OASISPackage.schema 
    plugin_id 
    fn 
    ~default:true 
    boolean
    (Printf.sprintf (f_ "Enable %s file generation.") fn),
  new_field 
    OASISPackage.schema
    plugin_id 
    (fn^"Filename")
    ~default:(fn^".txt")
    string_not_empty
    (Printf.sprintf (f_ "Real filename to use for file %s.") fn)
;;

let readme =
  fn_enable "README"
;;

let install = 
  fn_enable "INSTALL"
;;

let authors =
  fn_enable "AUTHORS"
;;

open Format;;

let main pkg = 
  let data =
    pkg.schema_data
  in

  let nl fmt = 
    pp_print_newline fmt ()
  in

  let nl2 fmt =
    pp_print_cut fmt ();
    nl fmt
  in 

  let pp_print_para fmt str = 
    String.iter 
      (function
         | ' ' -> pp_print_space fmt ()
         | '\n' -> nl2 fmt
         | c -> pp_print_char fmt c)
      str;
    nl2 fmt
  in

  let pp_print_title fmt str =
    pp_print_string fmt str;
    nl fmt;
    pp_print_string fmt (String.make (String.length str) '=');
    nl2 fmt
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
         fprintf fmt 
           "This is the README file for the %s distribution."
           pkg.name;
         nl2 fmt;

         pp_print_para fmt pkg.synopsis;
         
         List.iter
           (pp_print_para fmt)
           pkg.copyrights;
         
         if install_enable data then
           (
             fprintf fmt
               "See the files %s for building and installation instructions."
               (install_fn data);
             nl2 fmt
           );

         (match pkg.homepage with 
            | Some url ->
                fprintf fmt
                  "Home page: %s" url;
                nl2 fmt
            | None -> 
                ());

         (match pkg.description with 
            | Some str ->
                pp_print_para fmt str
            | None ->
                ());

         fprintf fmt "See the file %s for copying conditions." pkg.license_file);

    (* Generate INSTALL.txt *)
    file_generate install
      (fun fmt ->
         fprintf fmt 
           "This is the INSTALL file for the %s distribution."
           pkg.name;
         nl2 fmt;

         pp_print_para fmt
           "This package use ocaml-autobuild to generate its build system. \
            See section ocaml-autobuild for full information. ";

         pp_print_title fmt "Dependencies";
         pp_print_string fmt "In order to compile this package, you will need:";
         nl fmt;
         List.iter 
           (fprintf fmt "* %s\n")
           pkg.build_tools;
         List.iter 
           (function
              | FindlibPackage (lib, Some ver) -> 
                  fprintf fmt "* findlib package %s (%s)\n" lib ver
              | FindlibPackage (lib, None) -> 
                  fprintf fmt "* findlib package %s\n" lib
              | InternalLibrary _ ->
                  ())
           (* TODO: include depends for each package *)
           pkg.build_depends;

         pp_print_string fmt StdFilesGenData.install);
    
    (* Generate AUTHORS.txt *)
    file_generate authors
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt "Authors of %s@," pkg.name;

         List.iter
           (fprintf fmt "%s@,")
           pkg.authors;

         if pkg.maintainers <> [] then
           (
             nl2 fmt;
             fprintf fmt "Current maintainers of %s@," pkg.name;
             List.iter
               (fprintf fmt "%s@,")
               pkg.maintainers
           );

         pp_close_box fmt ()
      );
;;

plugin_register plugin_id (Extra main);;
