
(** Install using internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open BaseStandardVar
open OASISMessage
open OASISTypes
open OASISLibrary
open OASISGettext
open OASISUtils

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

  let in_destdir =
    try 
      let destdir =
        destdir () 
      in
        (* Practically speaking destdir is prepended
         * at the beginning of the target filename
         *)
        fun fn -> destdir^fn
    with PropList.Not_set _ ->
      fun fn -> fn
  in

  let install_file src_file envdir = 
    let tgt_dir = 
      in_destdir (envdir ())
    in
    let tgt_file =
      Filename.concat 
        tgt_dir
        (Filename.basename src_file)
    in
      (* Check that target directory exist *)
      if not (Sys.file_exists tgt_dir) then
        (
          info (f_ "Creating directory '%s'") tgt_dir;
          BaseFileUtil.mkdir tgt_dir;
          BaseLog.register install_dir_ev tgt_dir
        );

      (* Really install files *)
      info (f_ "Copying file '%s' to '%s'") src_file tgt_file;
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
                 failwithf1
                   (f_ "Wildcard '%s' doesn't match any files")
                   src;
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
                 (Filename.concat 
                    (datarootdir ())
                    pkg.name)
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

    let files_of_library acc data_lib = 
      let cs, bs, lib, lib_extra =
        !lib_hook data_lib
      in
        if var_choose bs.bs_install then
          begin
            let acc = 
              (* Start with acc + lib_extra *)
              List.rev_append lib_extra acc
            in
            let acc = 
              (* Add uncompiled header from the source tree *)
              let path = 
                BaseFilePath.of_unix bs.bs_path
              in
                List.fold_left
                  (fun acc modul ->
                     try 
                       List.find
                         Sys.file_exists 
                         (List.map
                            (Filename.concat path)
                            [String.uncapitalize modul^".mli";
                             String.capitalize   modul^".mli";
                             String.uncapitalize modul^".ml";
                             String.capitalize   modul^".ml"])
                       :: acc
                     with Not_found ->
                       begin
                         warning 
                           (f_ "Cannot find source header for module %s \
                                in library %s")
                           modul cs.cs_name;
                         acc
                       end)
                  acc
                  lib.lib_modules
            in
             (* Get generated files *)
             BaseBuilt.fold 
               BaseBuilt.BLib 
               cs.cs_name
               (fun acc fn -> fn :: acc)
               acc
          end
         else
          begin
            acc
          end
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
        install_group_lib_aux [] grp
      in

        (* Really install, if there is something to install *)
        if files = [] then 
          begin
            warning
              (f_ "Nothing to install for findlib library '%s'")
              findlib_name
          end
        else
          begin
            let meta = 
              (* Search META file *)
              let (_, bs, _) = 
                root_lib
              in
              let res = 
                Filename.concat bs.bs_path "META"
              in
                if not (Sys.file_exists res) then
                  failwithf2
                    (f_ "Cannot find file '%s' for findlib library %s")
                    res
                    findlib_name;
                res
            in
              info 
                (f_ "Installing findlib library '%s'")
                findlib_name;
              BaseExec.run 
                (ocamlfind ()) 
                ("install" :: findlib_name :: meta :: files);
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
      let (cs, bs, exec) =
        !exec_hook data_exec
      in
        if var_choose bs.bs_install then
          begin
            let exec_libdir () =
              Filename.concat 
                (libdir ())
                pkg.name
            in
              BaseBuilt.fold
                BaseBuilt.BExec
                cs.cs_name
                (fun () fn ->
                   install_file
                     fn
                     bindir)
                ();
              BaseBuilt.fold
                BaseBuilt.BExecLib
                cs.cs_name
                (fun () fn ->
                   install_file
                     fn
                     exec_libdir)
                ()
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
         begin
           if Sys.file_exists data then
             begin
               info
                 (f_ "Removing file '%s'")
                 data;
               Sys.remove data
             end
         end 
       else if ev = install_dir_ev then
         begin
           if Sys.file_exists data && Sys.is_directory data then
             begin
               if Sys.readdir data = [||] then
                 begin
                   info
                     (f_ "Removing directory '%s'")
                     data;
                   BaseFileUtil.rmdir data
                 end
               else
                 begin
                   warning 
                     (f_ "Directory '%s' is not empty (%s)")
                     data
                     (String.concat 
                        ", " 
                        (Array.to_list 
                           (Sys.readdir data)))
                 end
             end
         end
       else if ev = install_findlib_ev then
         begin
           info (f_ "Removing findlib library '%s'") data;
           BaseExec.run (ocamlfind ()) ["remove"; data]
         end
       else
         failwithf1 (f_ "Unknown log event '%s'") ev;
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
