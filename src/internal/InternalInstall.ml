
(** Install using ocaml-autobuild internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnvRW;;
open BaseStandardVar;;

type comp_type =
  | Byte
  | Native
  | Best 
;;

type library =
    {
      lib_name:            string;
      lib_install:         bool BaseExpr.choices;
      lib_modules:         string list;
      lib_path:            string;
      lib_extra:           string list;
      lib_c_sources:       bool;
      lib_compiled_object: comp_type;
    }
;;

type executable =
    {
      exec_path:            string;
      exec_name:            string;
      exec_filename:        string;
      exec_install:         bool BaseExpr.choices;
      exec_c_sources:       bool;
      exec_custom:          bool;
      exec_compiled_object: comp_type; 
    }
;;

let srcdir =
  var_define
    "srcdir"
    (lazy ".")
;;

let builddir env =
  var_define
    "builddir"
    (lazy (Filename.concat (srcdir env) "_build"))
    env
;;

let dllfn path name env = 
  Filename.concat path ("dll"^name^(ext_dll env))
;;

let libfn path name env =
  Filename.concat path ("lib"^name^(ext_lib env))
;;

let exec_hook =
  ref (fun env exec -> exec)
;;

let lib_hook =
  ref (fun env lib -> lib)
;;

let install libs execs env argv =
  
  let rootdirs =
    [srcdir env; builddir env]
  in

  let ( * ) lst1 lst2 = 
    List.flatten 
      (List.map 
         (fun a -> 
            List.map 
              (fun b -> a,b) 
              lst2) 
         lst1)
  in

  let make_filename =
    function
      | [] -> "" 
      | hd :: tl  -> List.fold_left Filename.concat hd tl
  in

  let make_module nm = 
    [String.capitalize nm; String.uncapitalize nm]
  in

  let find_file f lst = 
    let alternatives =
      List.map (fun e -> make_filename (f e)) lst
    in
      try 
        List.find Sys.file_exists alternatives
      with Not_found ->
        failwith 
          (Printf.sprintf 
             "Cannot find any of the files: %s"
             (String.concat ", " 
                (List.map 
                   (Printf.sprintf "%S")
                   alternatives)))
  in

  let find_build_file fn =
    find_file
      (fun rootdir -> [rootdir; fn])
      rootdirs
  in

  let is_native comp_type env =
    match comp_type with 
      | Best -> (ocamlbest env) = "native" 
      | Byte -> false
      | Native -> true
  in

  let install_lib env lib = 
    let lib =
      !lib_hook env lib
    in
    let install =
      BaseExpr.choose lib.lib_install env
    in
      if install then
        (
          let find_lib_file fn =
            find_file
              (fun rootdir -> [rootdir; lib.lib_path; fn])
              rootdirs
          in

          let module_to_cmi modul =
            find_file 
               (fun (rootdir, fn) -> [rootdir; lib.lib_path; (fn^".cmi")])
               (rootdirs * (make_module modul))
          in

          let module_to_header modul =
            assert(modul <> "");
            find_file 
               (fun ((rootdir, fn), ext) -> [rootdir; lib.lib_path; fn^ext])
               (rootdirs * (make_module modul) * [".mli"; ".ml"])
          in
            
          let files =
            List.flatten
              (
                [
                  find_lib_file "META";
                  find_lib_file (lib.lib_name^".cma");
                ]
                :: 
                (if is_native lib.lib_compiled_object env then
                   (
                     try 
                       [
                         find_lib_file (lib.lib_name^".cmxa");
                         find_lib_file (lib.lib_name^(ext_lib env));
                       ]
                     with Failure txt ->
                       BaseMessage.warning 
                         (Printf.sprintf
                            "Cannot install native library %s: %s"
                            lib.lib_name
                            txt);
                       []
                   )
                 else
                   []
                )
                ::
                lib.lib_extra
                ::
                (if lib.lib_c_sources then
                   [
                     find_build_file (libfn lib.lib_path lib.lib_name env);
                   ]
                 else
                   [])
                ::
                (* Some architecture doesn't allow shared library (Cygwin, AIX) *)
                (if lib.lib_c_sources then
                   (try 
                     [
                       find_build_file (dllfn lib.lib_path lib.lib_name env);
                     ]
                    with Failure txt ->
                      if (os_type env ) <> "Cygwin" then
                        BaseMessage.warning
                          (Printf.sprintf
                             "Cannot install C static library %s: %s"
                             lib.lib_name
                             txt);
                      [])
                 else
                   [])
                ::
                (
                  List.rev_map
                    (fun modul -> [module_to_cmi modul; module_to_header modul])
                    lib.lib_modules
                )
              )
          in
            BaseExec.run "ocamlfind" ("install" :: lib.lib_name :: files)
        )
  in

  let install_exec env exec =
    let exec =
      !exec_hook env exec
    in
    let install src_file envdir = 
      let tgt_file =
        Filename.concat 
          (envdir env)
          (Filename.basename src_file)
      in
        BaseMessage.info 
          (Printf.sprintf 
             "Copying file %s to %s"
             src_file
             tgt_file);
        BaseFileUtil.cp src_file tgt_file
    in
      if BaseExpr.choose exec.exec_install env then
        (
          let () = 
            install 
              (find_build_file
                 (exec.exec_filename^(suffix_program env)))
              bindir
          in
            if exec.exec_c_sources && 
               not exec.exec_custom && 
               not (is_native exec.exec_compiled_object env) then
              (
                install
                  (find_build_file
                     (dllfn exec.exec_path exec.exec_name env))
                  libdir
              )
            else
              ()
        )
  in

    List.iter (install_lib env) libs;
    List.iter (install_exec env) execs
;;

(* END EXPORT *)

open BaseExpr;;
open BaseGenCode;;

module OASIS = OASISTypes;;

let compiled_object_of_oasis comp_obj = 
  let vrt = 
    match comp_obj with
      | OASIS.Best -> "Best"
      | OASIS.Byte -> "Byte"
      | OASIS.Native -> "Native"
  in
    VRT ("InternalInstall."^vrt, [])
;;

let library_code_of_oasis (nm, lib) =
  REC 
    ("InternalInstall",
     ["lib_name",             STR nm;
      "lib_install",          code_of_bool_choices 
                                ((choices_of_oasis lib.OASIS.lib_build)
                                @
                                 (choices_of_oasis lib.OASIS.lib_install));
      "lib_modules",          LST (List.map 
                                     (fun s -> STR s) 
                                     lib.OASIS.lib_modules);
      "lib_path",             STR lib.OASIS.lib_path;
      "lib_extra",            LST [];
      "lib_c_sources",        BOO (lib.OASIS.lib_c_sources <> []);
      "lib_compiled_object",  compiled_object_of_oasis 
                                lib.OASIS.lib_compiled_object];
    );
;;

let executable_code_of_oasis (nm, exec) = 
  REC 
    ("InternalInstall",
     ["exec_path",            STR (Filename.dirname exec.OASIS.exec_is);
      "exec_name",            STR nm;
      "exec_filename",        STR exec.OASIS.exec_is;
      "exec_install",         code_of_bool_choices 
                                ((choices_of_oasis exec.OASIS.exec_build)
                                @
                                 (choices_of_oasis exec.OASIS.exec_install));
      "exec_c_sources",       BOO (exec.OASIS.exec_c_sources <> []);
      "exec_custom",          BOO exec.OASIS.exec_custom;
      "exec_compiled_object", compiled_object_of_oasis 
                                exec.OASIS.exec_compiled_object;])
;;
