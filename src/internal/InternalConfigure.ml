
(** Configure using ocaml-autobuild internal scheme
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

  let build_checks cond tools depends =
    if var_choose cond then
      begin
        (* Check tools *)
        List.iter 
          (fun tool -> var_ignore_eval (BaseCheck.prog tool))
          tools;

        (* Check depends *)
        List.iter  
          (function
             | FindlibPackage (findlib_pkg, version_comparator) ->
                 var_ignore_eval
                   (BaseCheck.package ?version_comparator findlib_pkg)
             | InternalLibrary nm ->
                 begin
                   let lib = 
                     try
                       List.assoc nm pkg.libraries 
                     with Not_found ->
                       failwith
                         (Printf.sprintf
                            "Cannot find internal library '%s' \
                             when checking build depends"
                            nm)
                   in
                     if not (var_choose lib.lib_build) then
                       failwith
                         (Printf.sprintf
                            "Internal library '%s' won't be built"
                            nm)
                 end)
          depends
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
    moduls       = [InternalData.internalsys_ml];
    setup        = func configure "InternalConfigure.configure";
    clean        = None;
    distclean    = None;
    other_action = (fun _ -> ());
  },
  pkg
