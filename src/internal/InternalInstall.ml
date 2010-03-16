
(** Install using internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open BaseStandardVar
open OASISTypes
open OASISLibrary

let srcdir =
  var_define
    "srcdir"
    (lazy ".")

let builddir =
  var_define
    "builddir"
    (lazy (Filename.concat (srcdir ()) "_build"))

let dllfn path name = 
  Filename.concat path ("dll"^name^(ext_dll ()))

let libfn path name =
  Filename.concat path ("lib"^name^(ext_lib ()))

let exec_hook =
  ref (fun (cs, bs, exec) -> cs, bs, exec)

let lib_hook =
  ref (fun (cs, bs, lib) -> cs, bs, lib, [])

let doc_hook =
  ref (fun (cs, doc) -> cs, doc)

let install_file_ev = 
  "install-file"

let install_dir_ev =
  "install-dir"

let install_findlib_ev =
  "install-findlib"

let install pkg argv =
  let rootdirs =
    [srcdir (); builddir ()]
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

  let is_native comp_type =
    match comp_type with 
      | Best -> (ocamlbest ()) = "native" 
      | Byte -> false
      | Native -> true
  in

  let install_file src_file envdir = 
    let tgt_dir = 
      envdir ()
    in
    let tgt_file =
      Filename.concat 
        tgt_dir
        (Filename.basename src_file)
    in
      (* Check that target directory exist *)
      if not (Sys.file_exists tgt_dir) then
        (
          BaseMessage.info 
            (Printf.sprintf 
               "Creating directory '%s'"
               tgt_dir);
          BaseFileUtil.mkdir tgt_dir;
          BaseLog.register install_dir_ev tgt_dir
        );

      (* Really install files *)
      BaseMessage.info 
        (Printf.sprintf 
           "Copying file '%s' to '%s'"
           src_file
           tgt_file);
      BaseFileUtil.cp src_file tgt_file;
      BaseLog.register install_file_ev tgt_file
  in

  (* Install all datas *)
  let install_datas pkg = 

    (* Install data into defined directory *)
    let install_data srcdir lst tgtdir =
      let tgtdir = 
        var_expand tgtdir
      in
        List.iter
          (fun (src, tgt_opt) ->
             let real_srcs = 
               BaseFileUtil.glob 
                 (Filename.concat srcdir src)
             in
               if real_srcs = [] then
                 failwith 
                   (Printf.sprintf 
                      "Wildcard '%s' doesn't match any files"
                      src);
               List.iter 
                 (fun fn -> 
                    install_file 
                      fn 
                      (fun () -> 
                         match tgt_opt with 
                           | Some s -> var_expand s
                           | None -> tgtdir))
                 real_srcs)
          lst
    in       

      List.iter
        (function
           | Library (_, bs, _)
           | Executable (_, bs, _) ->
               install_data
                 bs.bs_path
                 bs.bs_data_files
                 "$datarootdir/$pkg_name"
           | Doc (_, doc) ->
               install_data
                 Filename.current_dir_name
                 doc.doc_data_files
                 doc.doc_install_dir
           | _ ->
               ())
        pkg.sections
  in

  (** Install all libraries *)
  let install_libs pkg =

    let find_lib_file (cs, bs, lib) fn =
      find_file
        (fun rootdir -> [rootdir; bs.bs_path; fn])
        rootdirs
    in

    let files_of_library acc data_lib = 
      let cs, bs, lib, lib_extra =
        !lib_hook data_lib
      in
      let find_lib_file =
        find_lib_file (cs, bs, lib)
      in
        (if var_choose bs.bs_install then
           [
             find_lib_file (cs.cs_name^".cma");
           ]
           :: 
           (if is_native bs.bs_compiled_object then
              (
                try 
                  [
                    find_lib_file (cs.cs_name^".cmxa");
                    find_lib_file (cs.cs_name^(ext_lib ()));
                  ]
                with Failure txt ->
                  BaseMessage.warning 
                    (Printf.sprintf
                       "Cannot install native library %s: %s"
                       cs.cs_name
                       txt);
                  []
              )
            else
              []
           )
           ::
           lib_extra
           ::
           (if bs.bs_c_sources <> [] then
              [
                find_build_file (libfn bs.bs_path cs.cs_name);
              ]
            else
              [])
           ::
           (* Some architecture doesn't allow shared library (Cygwin, AIX) *)
           (if bs.bs_c_sources <> [] then
              (try 
                [
                  find_build_file (dllfn bs.bs_path cs.cs_name);
                ]
               with Failure txt ->
                 if (os_type ()) <> "Cygwin" then
                   BaseMessage.warning
                     (Printf.sprintf
                        "Cannot install C static library %s: %s"
                        cs.cs_name
                        txt);
                 [])
            else
              [])
           ::
           (
             let module_to_cmi modul =
               find_file 
                  (fun (rootdir, fn) -> [rootdir; bs.bs_path; (fn^".cmi")])
                  (rootdirs * (make_module modul))
             in

             let module_to_header modul =
               assert(modul <> "");
               find_file 
                  (fun ((rootdir, fn), ext) -> [rootdir; bs.bs_path; fn^ext])
                  (rootdirs * (make_module modul) * [".mli"; ".ml"])
             in
               List.fold_left
                 (fun acc modul -> 
                    module_to_cmi modul :: module_to_header modul :: acc)
                 []
                 lib.lib_modules
           )
           ::
           acc
         else
           acc)
    in

    (* Install one group of library *)
    let install_group_lib grp = 
      (* Iterate through all group nodes *)
      let rec install_group_lib_aux acc grp =
        let acc, children = 
          match grp with 
            | Container (_, children) ->
                acc, children
            | Package (_, cs, bs, lib, children) ->
                files_of_library acc (cs, bs, lib), children
        in
          List.fold_left
            install_group_lib_aux
            acc
            children
      in

      (* Findlib name of the root library *)
      let findlib_name =
        findlib_of_group grp
      in

      (* Determine root library *)
      let root_lib =
        root_of_group grp
      in

      (* All files to install for this library *)
      let files =
        List.flatten (install_group_lib_aux [] grp)
      in

        (* Really install, if there is something to install *)
        if files = [] then 
          begin
            BaseMessage.info 
              (Printf.sprintf 
                 "Nothing to install for findlib library '%s'"
                 findlib_name)
          end
        else
          begin
            let meta = 
              find_lib_file root_lib "META"
            in
              BaseMessage.info 
                (Printf.sprintf
                   "Installing findlib library '%s'"
                   findlib_name);
              BaseExec.run (ocamlfind ()) ("install" :: findlib_name :: meta :: files);
              BaseLog.register install_findlib_ev findlib_name 
          end
    in

      (* We install libraries in groups *)
      List.iter 
        install_group_lib
        (group_libs pkg)
  in

  let install_execs pkg = 
    let install_exec data_exec =
      let (cs, bs, exec) as data_exec =
        !exec_hook data_exec
      in
        if var_choose bs.bs_install then
          begin
            install_file
              (find_build_file
                 ((OASISExecutable.exec_is data_exec)^(suffix_program ())))
              bindir;
            
            if bs.bs_c_sources <> [] && 
               not exec.exec_custom && 
               (* TODO: move is_native to OASISBuildSection *)
               not (is_native bs.bs_compiled_object) then
              begin
                install_file
                  (find_build_file
                     (dllfn (OASISExecutable.exec_main_path data_exec) cs.cs_name))
                  libdir
              end
          end
    in
      List.iter
        (function
           | Executable (cs, bs, exec)->
               install_exec (cs, bs, exec)
           | _ ->
               ())
        pkg.sections
  in

  let install_docs pkg = 
    let install_doc data =
      let (cs, doc) =
        !doc_hook data
      in
        if var_choose doc.doc_install then
          begin
            let tgt_dir =
              var_expand doc.doc_install_dir
            in
              BaseBuilt.fold
                BaseBuilt.BDoc
                cs.cs_name
                (fun () fn ->
                   install_file 
                     fn 
                     (fun () -> tgt_dir))
              ()
          end
    in
      List.iter
        (function
           | Doc (cs, doc) ->
               install_doc (cs, doc)
           | _ ->
               ())
        pkg.sections
  in
  
    install_libs  pkg;
    install_execs pkg;
    install_docs  pkg;
    install_datas pkg

(* Uninstall already installed data *)
let uninstall _ argv =
  List.iter 
    (fun (ev, data) ->
       if ev = install_file_ev then
         (
           if Sys.file_exists data then
             (
               BaseMessage.info
                 (Printf.sprintf 
                    "Removing file '%s'"
                    data);
               Sys.remove data
             )
         )
       else if ev = install_dir_ev then
         (
           if Sys.file_exists data && Sys.is_directory data then
             (
               if Sys.readdir data = [||] then
                 (
                   BaseMessage.info
                     (Printf.sprintf 
                        "Removing directory '%s'"
                        data);
                   BaseFileUtil.rmdir data
                 )
               else
                 (
                   BaseMessage.warning 
                     (Printf.sprintf
                        "Directory '%s' is not empty (%s)"
                        data
                        (String.concat 
                           ", " 
                           (Array.to_list 
                              (Sys.readdir data))))
                 )
             )
         )
       else if ev = install_findlib_ev then
         (
           BaseMessage.info
             (Printf.sprintf
                "Removing findlib library '%s'"
                data);
           BaseExec.run (ocamlfind ()) ["remove"; data]
         )
       else
         (
           failwith (Printf.sprintf "Unknown log event '%s'" ev)
         );
       BaseLog.unregister ev data)
    (* We process event in reverse order *)
    (List.rev 
       (BaseLog.filter 
          [install_file_ev; 
           install_dir_ev;
           install_findlib_ev;]))

(* END EXPORT *)

open OASISPlugin

let () = 
  let module PU = Install.Make(InternalId)
  in
  (* Installation *)
  let doit_install pkg =
      {
        moduls       = [InternalData.internalsys_ml];
        setup        = ODNFunc.func install "InternalInstall.install";
        clean        = None;
        distclean    = None;
        other_action = ignore;
      },
      pkg
  in

  (* Uninstall *)
  let doit_uninstall pkg = 
    {
      moduls       = [InternalData.internalsys_ml];
      setup        = ODNFunc.func uninstall "InternalInstall.uninstall";
      clean        = None;
      distclean    = None;
      other_action = ignore;
    },
    pkg
  in

    PU.register (doit_install, doit_uninstall)
