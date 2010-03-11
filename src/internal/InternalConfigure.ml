
(** Configure using internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes

(** Configure build using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg argv =
  let var_ignore_eval var = 
    let _s : string =
      var ()
    in 
      ()
  in

  let build_checks bs =
    if var_choose bs.bs_build then
      begin
        (* Check tools *)
        List.iter 
          (function
             | ExternalTool tool -> 
                 var_ignore_eval (BaseCheck.prog tool)
             | InternalExecutable nm1 ->
                 (* Check that matching tool is built *)
                 List.iter
                   (function
                      | Executable ({cs_name = nm2}, 
                                    {bs_build = build}, 
                                    _) when nm1 = nm2 ->
                           if not (var_choose build) then
                             failwith 
                               (Printf.sprintf
                                  "Cannot find buildable internal executable \
                                   '%s' when checking build depends"
                                  nm1)
                           else
                             ()
                      | _ ->
                          ())
                   pkg.sections)
          bs.bs_build_tools;

        (* Check depends *)
        List.iter  
          (function
             | FindlibPackage (findlib_pkg, version_comparator) ->
                 var_ignore_eval
                   (BaseCheck.package ?version_comparator findlib_pkg)
             | InternalLibrary nm1 ->
                 (* Check that matching library is built *)
                 List.iter
                   (function
                      | Library ({cs_name = nm2},
                                 {bs_build = build}, 
                                 _) when nm1 = nm2 ->
                           if not (var_choose build) then
                             failwith 
                               (Printf.sprintf
                                  "Cannot find buildable internal library \
                                   '%s' when checking build depends"
                                  nm1)
                           else
                             ()
                      | _ ->
                          ())
                   pkg.sections)
          bs.bs_build_depends
      end
  in

  let ver_opt_check prefix std_var  =
    function
      | Some ver_cmp ->
          var_ignore_eval
            (BaseCheck.version prefix ver_cmp std_var)
      | None ->
          ()
  in


  (* Parse command line *)
  BaseArgExt.parse argv (args ());

  (* OCaml version *)
  ver_opt_check "ocaml" BaseStandardVar.ocaml_version pkg.ocaml_version;

  (* Findlib version *)
  ver_opt_check "findlib" BaseStandardVar.findlib_version pkg.findlib_version;

  (* Check build depends *)
  List.iter
    (function
       | Executable (_, bs, _)
       | Library (_, bs, _) ->
           build_checks bs
       | _ ->
           ())
    pkg.sections;

  (* Save and print environment *)
  dump ();
  print ()

(* END EXPORT *)

open OASISPlugin

let () =
  let module PU = Configure.Make(InternalId)
  in
  let doit pkg =  
    {
      moduls       = [InternalData.internalsys_ml];
      setup        = ODNFunc.func configure "InternalConfigure.configure";
      clean        = None;
      distclean    = None;
      other_action = (fun _ -> ());
    },
    pkg
  in
    PU.register doit
