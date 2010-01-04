
(** Configure using ocaml-autobuild internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv;;
open BaseExpr;;

(** Configure build using provided series of check to be done
  * and then output corresponding file.
  *)
let configure cond_checks argv =

  (* Parse command line *)
  BaseArgExt.parse argv (args ());

  (* Do some check *)
  List.iter
    (fun (cond, checks) ->
       if BaseExpr.choose cond then
         BaseCheck.run checks)
    cond_checks;

  dump ();
  print ()
;;

(* END EXPORT *)

open OASISTypes;;
open BaseGenCode;;
open BasePlugin;;

let code_of_oasis pkg =

  let build_depends_check acc = 
    function
      | FindlibPackage (findlib_pkg, ver_opt) ->
          (
            let version_arg = 
              match ver_opt with
                | Some ver ->
                    let cmp = 
                      BaseVersion.comparator_of_string ver
                    in
                      [
                        "version_comparator",
                        TPL [STR ver; 
                             BaseVersion.code_of_comparator cmp;
                             STR (BaseVersion.varname_of_comparator cmp)];
                      ]
                | None ->
                    []
            in
              APP ("BaseCheck.package", version_arg, [STR findlib_pkg]) :: acc
          )
      | InternalLibrary _ ->
          acc
  in

  let build_tools_check acc prog =
    APP ("BaseCheck.prog", [], [STR prog]) :: acc
  in

  let build_checks cond tools depends =
    TPL
      [
        (BaseExpr.code_of_bool_choices cond);
        LST
          (List.rev
             (List.fold_left 
                build_depends_check
                (List.fold_left
                   build_tools_check
                   []
                   tools)
                depends))
      ]
  in

  let build_depends_collect pkg =
   (build_checks 
      BaseExpr.condition_true
      pkg.build_tools 
      pkg.build_depends)
   ::
   (List.map 
     (fun (_, lib) -> 
        build_checks
          (BaseExpr.choices_of_oasis lib.lib_build)
          lib.lib_build_tools
          lib.lib_build_depends)
     pkg.libraries)
   @
   (List.map 
      (fun (_, exec) -> 
         build_checks 
           (BaseExpr.choices_of_oasis exec.exec_build)
           exec.exec_build_tools
           exec.exec_build_depends)
      pkg.executables)
  in

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

  let code_checks = 
    LST (build_depends_collect pkg @ ocaml_version_check pkg)
  in

    APP ("InternalConfigure.configure", [], [code_checks])
;;

(* Configuration *)
let plugin_main pkg =
  let code = 
    code_of_oasis pkg
  in
    {
      moduls = 
        [
          CommonData.commonsys_ml;
          BaseData.basesys_ml; 
          InternalData.internalsys_ml
        ];
      setup_code       = code;
      clean_code       = [];
      distclean_code   = [];
      other_action     = (fun _ -> ());
      files_generated  = (List.map BaseFileAB.to_filename pkg.files_ab);
    }
;;

