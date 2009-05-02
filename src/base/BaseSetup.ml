
(** Entry point for ocaml-autobuild
    @author Sylvain Le Gall
  *)

module Env = BaseEnvironment
;;

type action_fun = Env.env -> string array -> unit;;

type t =
    {
      configure:       action_fun;
      build:           action_fun;
      doc:             action_fun;
      test:            action_fun;
      install:         action_fun;
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
    (Env.filename :: t.files_generated);
  t.distclean ()
;;

let setup t = 
  try
    let act =
      ref (fun _ _ -> 
             failwith
               (Printf.sprintf
                  "No action defined, run '%s %s -help'"
                  Sys.executable_name
                  Sys.argv.(0)))

    in
    let is_configure =
      ref false
    in
    let args =
      ref []
    in
    let arg_rest ?(configure=false) a =
      Arg.Tuple
        [
          Arg.Rest (fun str -> args := str :: !args);
          Arg.Unit (fun () -> is_configure := configure;
                              act := a; 
                              args := List.rev !args);
        ]
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

          "-clean",
          arg_rest (fun _ _ -> t.clean ()),
          "[options*] Clean build environment.";

          "-distclean",
          arg_rest (fun _ _ -> distclean t),
          "[options*] Clean build and configure environment.";
        ]
        (fun str -> failwith ("Don't know what to do with "^str))
        "Setup and run build process current package\n";

      (* Build initial environment *)
      let env_org =
        Env.load ~allow_empty:!is_configure ()
      in
        !act env_org (Array.of_list !args)
  with e ->
    BaseMessage.error (Printexc.to_string e);
;;
