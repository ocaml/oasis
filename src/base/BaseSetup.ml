
(** Entry point for ocaml-autobuild
    @author Sylvain Le Gall
  *)

open BaseEnvRW;;

type action_fun = env_t -> string array -> unit;;

type t =
    {
      configure:       action_fun;
      build:           action_fun;
      doc:             action_fun;
      test:            action_fun;
      install:         action_fun;
      uninstall:       action_fun;
      clean:           unit -> unit;
      distclean:       unit -> unit;
      files_generated: string list;
    }
;;

let distclean t =
  (* Call clean *)
  t.clean ();
  (* Remove generated file *)
  List.iter
    (fun fn ->
       if Sys.file_exists fn then
         (BaseMessage.info 
            (Printf.sprintf "Remove '%s'" fn);
          Sys.remove fn))
    (BaseEnvRO.default_filename 
     :: 
     BaseLog.default_filename
     ::
     t.files_generated);
  t.distclean ()
;;

let setup t = 
  try
    let act =
      ref (fun () -> 
             failwith
               (Printf.sprintf
                  "No action defined, run '%s %s -help'"
                  Sys.executable_name
                  Sys.argv.(0)))

    in
    let args =
      ref []
    in
    let arg_rest ?(configure=false) a =
      Arg.Tuple
        [
          Arg.Rest (fun str -> args := str :: !args);
          Arg.Unit 
            (fun () ->
               (* Build initial environment *)
               let env_org =
                 load ~allow_empty:configure ()
               in
                 act :=
                 (let args =
                    !args 
                  in
                    fun () -> a env_org (Array.of_list (List.rev args)));
                 args := []); 
        ]
    in
    let arg_clean a =
      Arg.Unit (fun () -> act := a);
    in
      Arg.parse 
        [
          "-configure",
          arg_rest ~configure:true t.configure,
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

          "-uninstall",
          arg_rest t.uninstall,
          "[options*] Uninstall library, data, executable and documentation.";

          "-clean",
          arg_clean t.clean,
          "[options*] Clean build environment.";

          "-distclean",
          arg_clean (fun () -> distclean t),
          "[options*] Clean build and configure environment.";
        ]
        (fun str -> failwith ("Don't know what to do with "^str))
        "Setup and run build process current package\n";

        !act ()
  with e ->
    BaseMessage.error (Printexc.to_string e);
;;
