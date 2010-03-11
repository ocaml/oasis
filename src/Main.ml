
(** Main for OASIS *)

open Format
open OASISGettext
open OASISTypes
open OASISUtils
open OASISPlugin
open OASISBuiltinPlugins

type action_t =
  | Generate 
  | Quickstart

let () =

  let action =
    ref Generate
  in

  let oasis_fn =
    ref "_oasis"
  in

  let qckstrt_lvl =
    ref Beginner
  in

  let rdev =
    ref false
  in

  let rsetup_fn =
    ref "setup.ml"
  in

  let (gettext_args, _) =
    OASISGettext.init
  in

  let () = 
    try 
      Arg.parse_argv 
        Sys.argv
        (Arg.align 
           ([
             "-C",
             Arg.String (fun str -> Sys.chdir str),
             (s_ "dir Change directory before running.");

             "-quickstart",
             Arg.Unit (fun () -> action := Quickstart),
             (s_ " Launch an helper to write _oasis file.");

             (
               let lvls =
                 [
                   s_ "beginner", Beginner; 
                   s_ "intermediate", Intermediate; 
                   s_ "expert", Expert;
                 ]
               in
                 "-quickstart-level",
                 Arg.Symbol
                   ((List.map fst lvls),
                    (fun s -> qckstrt_lvl := List.assoc s lvls)),
                 (s_ " Quickstart level, skip questions according to this level.")
             );

             "-dev",
             Arg.Set rdev,
             (s_ " Create a developper mode setup.ml. It will be automatically \
                   updated at each run.");

             "-setup-fn",
             Arg.Set_string rsetup_fn,
             (s_ " Change the default name of setup.ml. This option should be \
                   used with caution, it is reserved for internal use.");                   

             "-quiet",
             Arg.Clear BaseMessage.verbose,
             (s_ " Run quietly");
           ] @ gettext_args))
        (fun str -> 
           failwith 
             (Printf.sprintf 
                (f_ "Don't know what to do with '%s'")
                str))
        (s_ "OASIS (C) 2009-2010 OCamlCore SARL\n\
             \n\
             OASIS [options*] -action [action-options*]\n\n");
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
                     OASISPlugin.Configure.ls ();
                     OASISPlugin.Build.ls ();
                     OASISPlugin.Doc.ls ();
                     OASISPlugin.Test.ls ();
                     OASISPlugin.Install.ls ();
                     OASISPlugin.Extra.ls ();
                   ]));

            pp_print_flush std_formatter ();
            exit 0

  in

    try 
      match !action with 
        | Generate ->
            begin
              let pkg =
                OASIS.from_file !oasis_fn 
              in
                BaseGenerate.generate pkg !rdev !rsetup_fn
            end
        | Quickstart ->
            begin
              let fn =
                "_oasis"
              in
              let chn = 
                open_out_gen 
                  [Open_wronly; Open_creat; Open_excl; Open_text] 
                  0o644
                  "_oasis"
              in
              let fmt = 
                Format.formatter_of_out_channel chn
              in
                Printf.printf "Creating %s file\n%!" fn;
                OASISQuickstart.quickstart 
                  fmt
                  !qckstrt_lvl;
                Format.pp_print_flush fmt ();
                close_out chn
            end
    with Failure s ->
      begin
        prerr_endline s;
        exit 1
      end
