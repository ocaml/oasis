
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

  (* Parse command line *)
  BaseArgExt.parse argv (args ());

  (* OCaml version *)
  begin
    match pkg.ocaml_version with 
      | Some ver_cmp ->
          var_ignore_eval
            (BaseCheck.version
               "ocaml"
               ver_cmp
               BaseStandardVar.ocaml_version)
      | None ->
          ()
  end;

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

open BasePlugin

(* Configuration *)
let plugin_main pkg =
  {
    moduls       = [InternalData.internalsys_ml];
    setup        = func configure "InternalConfigure.configure";
    clean        = None;
    distclean    = None;
    other_action = (fun _ -> ());
  },
  pkg
