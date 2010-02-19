
(** Base functions for writing myocamlbuild.ml
    @author Sylvain Le Gall
  *)

TYPE_CONV_PATH "OCamlbuildBase"

open Ocamlbuild_plugin

type dir = string with odn
type name = string with odn

type t =
    {
      lib_ocaml: (name * dir list * bool) list;
      lib_c:     (name * dir) list; 
    } with odn

let dispatch_combine lst =
  fun e ->
    List.iter 
      (fun dispatch -> dispatch e)
      lst 

let dispatch t = 
  function
    | Before_options ->
        let env = 
          BaseEnvLight.load ~filename:(Pathname.basename BaseEnvLight.default_filename) ()
        in
        let no_trailing_dot s =
          if String.length s >= 1 && s.[0] = '.' then
            String.sub s 1 ((String.length s) - 1)
          else
            s
        in
          List.iter
            (fun (opt, var) ->
               opt := no_trailing_dot (BaseEnvLight.var_get var env))
            [
              Options.ext_obj, "ext_obj";
              Options.ext_lib, "ext_lib";
              Options.ext_dll, "ext_dll";
            ]

    | After_rules -> 
        (* Declare OCaml libraries *)
        List.iter 
          (function
             | lib, [], extern ->
                 ocaml_lib ~extern lib;
             | lib, dir :: tl, extern ->
                 ocaml_lib ~extern ~dir:dir lib;
                 List.iter 
                   (fun dir -> 
                      flag 
                        ["ocaml"; "use_"^lib; "compile"] 
                        (S[A"-I"; P dir]))
                   tl)
          t.lib_ocaml;

        (* Declare C libraries *)
        List.iter
          (fun (lib, dir) ->
               (* Handle C part of library *)
               flag ["link"; "library"; "ocaml"; "byte"; "use_lib"^lib]
                 (S[A"-dllib"; A("-l"^lib); A"-cclib"; A("-l"^lib)]);

               flag ["link"; "library"; "ocaml"; "native"; "use_lib"^lib]
                 (S[A"-cclib"; A("-l"^lib)]);
                    
               flag ["link"; "program"; "ocaml"; "byte"; "use_lib"^lib]
                 (S[A"-dllib"; A("dll"^lib)]);

               (* When ocaml link something that use the C library, then one
                  need that file to be up to date.
                *)
               dep  ["link"; "ocaml"; "use_lib"^lib] 
                 [dir/"lib"^lib^"."^(!Options.ext_lib)];

               (* Setup search path for lib *)
               flag ["link"; "ocaml"; "use_"^lib] 
                 (S[A"-I"; P(dir)]);
          )
          t.lib_c
    | _ -> 
        ()

let dispatch_default t =
  dispatch_combine 
    [
      dispatch t;
      OCamlbuildFindlib.dispatch;
    ]

(* END EXPORT *)
