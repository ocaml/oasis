
(** Base functions for writing myocamlbuild.ml
    @author Sylvain Le Gall
  *)

open Ocamlbuild_plugin

type dir = string
type name = string

type t =
    {
      lib_ocaml: (name * dir list) list;
    }
;;

let dispatch_combine lst =
  fun e ->
    List.iter 
      (fun dispatch -> dispatch e)
      lst 
;;

let dispatch t = 
  function
    | Before_options ->
        let env = 
          BaseEnvRO.load ~filename:(Pathname.basename BaseEnvRO.default_filename) ()
        in
          Options.ext_obj := BaseEnvRO.var_get "ext_obj" env;
          Options.ext_lib := BaseEnvRO.var_get "ext_lib" env;
          Options.ext_dll := BaseEnvRO.var_get "ext_dll" env;

    | After_rules -> 
        (* Declare OCaml libraries *)
        List.iter 
          (function
             | lib, [] ->
                 ocaml_lib lib;
             | lib, dir :: tl ->
                 ocaml_lib ~dir:dir lib;
                 List.iter 
                   (fun dir -> 
                      flag 
                        ["ocaml"; "use_"^lib; "compile"] 
                        (S[A"-I"; P dir]))
                   tl)
          t.lib_ocaml

    | _ -> 
        ()
;;

let dispatch_default t =
  dispatch_combine 
    [
      dispatch t;
      OCamlbuildFindlib.dispatch;
    ]
;;
