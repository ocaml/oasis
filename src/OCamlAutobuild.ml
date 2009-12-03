
(** Main for OCaml-autobuild *)

open NoneGen;;
open InternalGen;;
open OCamlbuildGen;;
open CustomGen;;
open METAGen;;
open DevFilesGen;;
open StdFilesGen;;

open BaseGenerate;;
open BasePlugin;;
open BaseUtils;;
open CommonGettext;;
open Format;;

let () =

  let oasis_fn =
    ref "_oasis"
  in

  let () = 
    try 
      Arg.parse_argv 
        Sys.argv
        (Arg.align 
           [
             "-C",
             (Arg.String (fun str -> Sys.chdir str)),
             (s_ "dir Change directory before running.");
           ])
        (fun str -> 
           failwith 
             (Printf.sprintf 
                (f_ "Don't know what to do with '%s'")
                str))
        (s_ "ocaml-autobuild (C) 2009 Sylvain Le Gall\n\
             \n\
             ocaml-autobuild [options*] -action [action-options*]\n\n");
    with 
      | Arg.Bad txt ->
          prerr_endline txt;
          exit 1

      | Arg.Help txt ->
          let fmt = 
            std_formatter
          in
          let pp_print_title fmt str =
            pp_print_newline fmt ();
            pp_print_newline fmt ();
            pp_print_string fmt str;
            pp_print_newline fmt ();
            pp_print_newline fmt ()
          in
            pp_print_title fmt (s_ "= Command line options =");
            pp_print_string  fmt txt;

            pp_print_title fmt (s_ "= OASIS file layout =");
            OASIS.pp_help fmt ();

            pp_print_title fmt (s_ "= Plugins =");
            pp_print_string fmt "TODO";

            List.iter
              (fun plgn ->
                 pp_print_title fmt ((s_ "= Plugin ")^plgn^" =");
                 OASIS.pp_help fmt ~plugin:plgn ())
              (SetString.elements
                (List.fold_left
                   set_string_add_list
                   SetString.empty
                   [
                     plugin_ls configure_plugins;
                     plugin_ls build_plugins;
                     plugin_ls doc_plugins;
                     plugin_ls test_plugins;
                     plugin_ls install_plugins;
                     plugin_ls uninstall_plugins;
                     plugin_ls extra_plugins;
                   ]));

            pp_print_flush std_formatter ();
            exit 0

  in

  let pkg =
    OASIS.from_file !oasis_fn 
  in

    generate pkg;
;;
