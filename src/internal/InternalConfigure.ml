
(** Configure using ocaml-autobuild internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes

(** Configure build using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg argv =

  let build_checks cond tools depends =
    if var_choose cond then
      begin
        (* Check tools *)
        List.iter 
          (fun tool -> var_ignore (BaseCheck.prog tool))
          tools;

        (* Check depends *)
        List.iter  
          (function
             | FindlibPackage (findlib_pkg, ver_opt) ->
                 (* TODO: ver_opt *)
                 var_ignore (BaseCheck.package findlib_pkg)
             | InternalLibrary _ ->
                 (* TODO: check that matching library is built *)
                 ())
          depends
      end
  in

  (* Parse command line *)
  BaseArgExt.parse argv (args ());

  (* OCaml version *)
  (* TODO
  let ocaml_version_check pkg = 
    match pkg.ocaml_version with 
      | Some ver -> 
          (
            let cmp = 
              BaseVersion.comparator_of_string ver
            in
              [TPL
                 [BaseExpr.code_condition_true;
                  LST
                    [
                      APP ("BaseCheck.version",
                           [],
                           [
                             STR "ocaml";
                             TPL [STR ver; 
                                  BaseVersion.code_of_comparator cmp;
                                  STR (BaseVersion.varname_of_comparator cmp)];
                             VAR "BaseStandardVar.ocaml_version";
                           ])]]]
          )
      | None ->
          []
  in
   *)

  (* Check build depends *)
  build_checks 
    [EBool true, true] 
    pkg.build_tools 
    pkg.build_depends;

  List.iter 
    (fun (_, lib) ->
       build_checks
         lib.lib_build
         lib.lib_build_tools
         lib.lib_build_depends)
    pkg.libraries;

  List.iter
    (fun (_, exec) ->
       build_checks
         exec.exec_build
         exec.exec_build_tools
         exec.exec_build_depends)
    pkg.executables;

  (* Save and print environment *)
  dump ();
  print ()

(* END EXPORT *)

open BasePlugin

(* Configuration *)
let plugin_main pkg =
  {
    moduls           = [InternalData.internalsys_ml];
    setup            = func configure "InternalConfigure.configure";
    clean            = None;
    distclean        = None;
    other_action     = (fun _ -> ());
    (* TODO: remove files generated, useless when we have the package *)
    files_generated  = (List.map BaseFileAB.to_filename pkg.files_ab);
  },
  pkg
