(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

(** Install using internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open BaseStandardVar
open BaseMessage
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

  let install_file ?tgt_fn src_file envdir =
    let tgt_dir =
      in_destdir (envdir ())
    in
    let tgt_file =
      Filename.concat
        tgt_dir
        (match tgt_fn with
           | Some fn ->
               fn
           | None ->
               Filename.basename src_file)
    in
      (* Create target directory if needed *)
      BaseFileUtil.mkdir_parent
        (fun dn ->
           info (f_ "Creating directory '%s'") dn;
           BaseLog.register install_dir_ev dn)
        tgt_dir;

      (* Really install files *)
      info (f_ "Copying file '%s' to '%s'") src_file tgt_file;
      BaseFileUtil.cp src_file tgt_file;
      BaseLog.register install_file_ev tgt_file
  in

  (* Install data into defined directory *)
  let install_data srcdir lst tgtdir =
    let tgtdir =
      BaseFilePath.of_unix (var_expand tgtdir)
    in
      List.iter
        (fun (src, tgt_opt) ->
           let real_srcs =
             BaseFileUtil.glob
               (Filename.concat srcdir src)
           in
             if real_srcs = [] then
               failwithf
                 (f_ "Wildcard '%s' doesn't match any files")
                 src;
             List.iter
               (fun fn ->
                  install_file
                    fn
                    (fun () ->
                       match tgt_opt with
                         | Some s ->
                             BaseFilePath.of_unix (var_expand s)
                         | None ->
                             tgtdir))
               real_srcs)
        lst
  in

  (** Install all libraries *)
  let install_libs pkg =

    let files_of_library (f_data, acc) data_lib =
      let cs, bs, lib, lib_extra =
        !lib_hook data_lib
      in
        if var_choose bs.bs_install &&
           BaseBuilt.is_built BaseBuilt.BLib cs.cs_name then
          begin
            let acc =
              (* Start with acc + lib_extra *)
              List.rev_append lib_extra acc
            in
            let acc =
              if lib.lib_pack then
                acc
              else
                (* Add uncompiled header from the source tree (for non-packed libraries) *)
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
                              [modul^".mli";
                               modul^".ml";
                               String.uncapitalize modul^".mli";
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

            let acc =
             (* Get generated files *)
             BaseBuilt.fold
               BaseBuilt.BLib
               cs.cs_name
               (fun acc fn -> fn :: acc)
               acc
            in

            let f_data () =
              (* Install data associated with the library *)
              install_data
                bs.bs_path
                bs.bs_data_files
                (Filename.concat
                   (datarootdir ())
                   pkg.name);
              f_data ()
            in

              (f_data, acc)
          end
         else
          begin
            (f_data, acc)
          end
    in

    (* Install one group of library *)
    let install_group_lib grp =
      (* Iterate through all group nodes *)
      let rec install_group_lib_aux data_and_files grp =
        let data_and_files, children =
          match grp with
            | Container (_, children) ->
                data_and_files, children
            | Package (_, cs, bs, lib, children) ->
                files_of_library data_and_files (cs, bs, lib), children
        in
          List.fold_left
            install_group_lib_aux
            data_and_files
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
      let f_data, files =
        install_group_lib_aux (ignore, []) grp
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
                  failwithf
                    (f_ "Cannot find file '%s' for findlib library %s")
                    res
                    findlib_name;
                res
            in
            let files = 
              (* Make filename shorter to avoid hitting command max line length
               * too early, esp. on Windows.
               *)
              let remove_prefix p n =
                let plen = String.length p in
                let nlen = String.length n in
                  if plen <= nlen && String.sub n 0 plen = p then
                    begin
                      let fn_sep = 
                        if Sys.os_type = "Win32" then
                          '\\'
                        else
                          '/'
                      in
                      let cutpoint = plen +
                        (if plen < nlen && n.[plen] = fn_sep then 
                           1
                         else 
                           0)
                      in
                        String.sub n cutpoint (nlen - cutpoint)
                    end
                  else 
                    n
              in
                List.map (remove_prefix (Sys.getcwd ())) files 
            in
              info
                (f_ "Installing findlib library '%s'")
                findlib_name;
              BaseExec.run
                (ocamlfind ())
                ("install" :: findlib_name :: meta :: files);
              BaseLog.register install_findlib_ev findlib_name
          end;

        (* Install data files *)
        f_data ();

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
        if var_choose bs.bs_install &&
           BaseBuilt.is_built BaseBuilt.BExec cs.cs_name then
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
                     ~tgt_fn:cs.cs_name
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
                ();
              install_data
                bs.bs_path
                bs.bs_data_files
                (Filename.concat
                   (datarootdir ())
                   pkg.name)
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
        if var_choose doc.doc_install &&
           BaseBuilt.is_built BaseBuilt.BDoc cs.cs_name then
          begin
            let tgt_dir =
              BaseFilePath.of_unix (var_expand doc.doc_install_dir)
            in
              BaseBuilt.fold
                BaseBuilt.BDoc
                cs.cs_name
                (fun () fn ->
                   install_file
                     fn
                     (fun () -> tgt_dir))
              ();
              install_data
                Filename.current_dir_name
                doc.doc_data_files
                doc.doc_install_dir
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
    install_docs  pkg

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
           else
             begin
               warning
                 (f_ "File '%s' doesn't exist anymore")
                 data
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
           else
             begin
               warning
                 (f_ "Directory '%s' doesn't exist anymore")
                 data
             end
         end
       else if ev = install_findlib_ev then
         begin
           info (f_ "Removing findlib library '%s'") data;
           BaseExec.run (ocamlfind ()) ["remove"; data]
         end
       else
         failwithf (f_ "Unknown log event '%s'") ev;
       BaseLog.unregister ev data)
    (* We process event in reverse order *)
    (List.rev
       (BaseLog.filter
          [install_file_ev;
           install_dir_ev;
           install_findlib_ev;]))

(* END EXPORT *)

open OASISPlugin
open InternalId

let plugin =
  `Install, name, Some version

let init () =
  let self_id, _ =
    Install.create plugin
  in
  (* Installation *)
  let doit_install ctxt pkg =
    ctxt,
    {
      chng_moduls    = [InternalData.internalsys_ml];
      chng_main      = ODNFunc.func install "InternalInstallPlugin.install";
      chng_clean     = None;
      chng_distclean = None;
    }
  in

  (* Uninstall *)
  let doit_uninstall ctxt pkg =
    ctxt,
    {
      chng_moduls    = [InternalData.internalsys_ml];
      chng_main      = ODNFunc.func uninstall "InternalInstallPlugin.uninstall";
      chng_clean     = None;
      chng_distclean = None;
    }
  in
    InternalId.init ();
    Install.register_act self_id (doit_install, doit_uninstall)
