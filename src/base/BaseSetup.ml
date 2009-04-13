
(** Entry point for ocaml-autobuild
    @author Sylvain Le Gall
  *)

type action_fun = string array -> unit;;

type t =
    {
      configure:   action_fun;
      build:       action_fun;
      doc:         action_fun;
      test:        action_fun;
      install:     action_fun;
      clean:       action_fun;
      distclean:   action_fun;
    }
;;

let setup t = 
  let act =
    ref (fun _ -> 
           failwith
             (Printf.sprintf
                "No action defined, run '%s %s -help'"
                Sys.executable_name
                Sys.argv.(0)))

  in
  let args =
    ref []
  in
  let arg_rest a =
    Arg.Tuple
      [
        Arg.Rest (fun str -> args := str :: !args);
        Arg.Unit (fun () -> act := a; args := List.rev !args);
      ]
  in
    Arg.parse 
      [
        "-configure",
        arg_rest t.configure,
        "[options*] Configure build process.";

        "-build",
        arg_rest t.build,
        "[options*] Run build process.";

        "-doc",
        arg_rest t.doc,
        "[options*] Build documentation.";

        "-test",
        arg_rest t.test,
        "[options*] Build and run tests.";

        "-install",
        arg_rest t.install,
        "[options*] Install library, data, executable and documentation.";

        "-clean",
        arg_rest t.clean,
        "[options*] Clean build environment.";

        "-distclean",
        arg_rest t.distclean,
        "[options*] Clean build and configure environment.";
      ]
      (fun str -> failwith ("Don't know what to do with "^str))
      "Setup and run build process current package\n";

    !act (Array.of_list !args)
;;
