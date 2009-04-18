
(** Configure using OCaml-autobuild
    @author Sylvain Le Gall
  *)

module Msg = BaseMessage;;
module Env = BaseEnvironment;;

(* TODO: use it
(** Evaluate expression *)
let rec expr_eval ctxt =
  function 
    | ETrue  ->
        true
    | EFalse -> 
        false
    | ENot e -> 
        expr_eval ctxt e 
    | EAnd (e1, e2) ->
        (expr_eval ctxt e1) && (expr_eval ctxt e2)
    | EOr (e1, e2) -> 
        (expr_eval ctxt e1) || (expr_eval ctxt e2)
    | EFlag nm ->
        (
          (* TODO *)
          false
        )
    | ETest (nm, vl) ->
        (
          (* TODO *)
          false
        )
;;
 *)

(** Build environment using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg_name pkg_version args checks ab_files argv =

  (* Data file for setup.ml *)
  let fn =
    (Filename.chop_extension Sys.argv.(0))^".data"
  in

  (* Build initial environment *)
  let env_org =
    Env.load fn
  in
  let env = 
    List.fold_left
      (fun env (nm, vl) -> Env.var_define nm (fun env -> vl, env) env)
      env_org
      [
        "pkg_name", pkg_name;
        "pkg_version", pkg_version;
      ]
  in
  (* Parse command line *)
  let env =
    BaseArgExt.parse argv (BaseArgExt.default :: args) env
  in

  (* Do some check *)
  let env =
    BaseCheck.run checks env
  in

  (* Replace data in file *)
  let env =
    BaseFileAB.replace
      ab_files
      env
  in

    if not (Env.equal env_org env) then
      (
        Env.dump  fn env;
        Env.print env
      )
;;
