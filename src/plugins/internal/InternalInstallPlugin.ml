(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
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
open OASISFindlib
open OASISGettext
open OASISUtils


let exec_hook =
  ref (fun (cs, bs, exec) -> cs, bs, exec)


let lib_hook =
  ref (fun (cs, bs, lib) -> cs, bs, lib, [])


let obj_hook =
  ref (fun (cs, bs, obj) -> cs, bs, obj, [])


let doc_hook =
  ref (fun (cs, doc) -> cs, doc)


let install_file_ev =
  "install-file"


let install_dir_ev =
  "install-dir"


let install_findlib_ev =
  "install-findlib"


let win32_max_command_line_length = 8000


let split_install_command ocamlfind findlib_name meta files =
  if Sys.os_type = "Win32" then
    (* Arguments for the first command: *)
    let first_args = ["install"; findlib_name; meta] in
    (* Arguments for remaining commands: *)
    let other_args = ["install"; findlib_name; "-add"] in
    (* Extract as much files as possible from [files], [len] is
       the current command line length: *)
    let rec get_files len acc files =
      match files with
        | [] ->
          (List.rev acc, [])
        | file :: rest ->
          let len = len + 1 + String.length file in
          if len > win32_max_command_line_length then
            (List.rev acc, files)
          else
            get_files len (file :: acc) rest
    in
    (* Split the command into several commands. *)
    let rec split args files =
      match files with
        | [] ->
          []
        | _ ->
          (* Length of "ocamlfind install <lib> [META|-add]" *)
          let len =
            List.fold_left
              (fun len arg ->
                 len + 1 (* for the space *) + String.length arg)
              (String.length ocamlfind)
              args
          in
          match get_files len [] files with
            | ([], _) ->
              failwith (s_ "Command line too long.")
            | (firsts, others) ->
              let cmd = args @ firsts in
              (* Use -add for remaining commands: *)
              let () =
                let findlib_ge_132 =
                  OASISVersion.comparator_apply
                    (OASISVersion.version_of_string
                       (BaseStandardVar.findlib_version ()))
                    (OASISVersion.VGreaterEqual
                       (OASISVersion.version_of_string "1.3.2"))
                in
                if not findlib_ge_132 then
                  failwithf
                    (f_ "Installing the library %s require to use the \
                         flag '-add' of ocamlfind because the command \
                         line is too long. This flag is only available \
                         for findlib 1.3.2. Please upgrade findlib from \
                         %s to 1.3.2")
                    findlib_name (BaseStandardVar.findlib_version ())
              in
              let cmds = split other_args others in
              cmd :: cmds
    in
    (* The first command does not use -add: *)
    split first_args files
  else
    ["install" :: findlib_name :: meta :: files]


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
    OASISFileUtil.mkdir_parent
      ~ctxt:!BaseContext.default
      (fun dn ->
         info (f_ "Creating directory '%s'") dn;
         BaseLog.register install_dir_ev dn)
      tgt_dir;

    (* Really install files *)
    info (f_ "Copying file '%s' to '%s'") src_file tgt_file;
    OASISFileUtil.cp ~ctxt:!BaseContext.default src_file tgt_file;
    BaseLog.register install_file_ev tgt_file
  in

  (* Install data into defined directory *)
  let install_data srcdir lst tgtdir =
    let tgtdir =
      OASISHostPath.of_unix (var_expand tgtdir)
    in
    List.iter
      (fun (src, tgt_opt) ->
         let real_srcs =
           OASISFileUtil.glob
             ~ctxt:!BaseContext.default
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
                       OASISHostPath.of_unix (var_expand s)
                     | None ->
                       tgtdir))
           real_srcs)
      lst
  in

  let make_fnames modul sufx =
    List.fold_right
      begin fun sufx accu ->
        (String.capitalize modul ^ sufx) ::
          (String.uncapitalize modul ^ sufx) ::
          accu
      end
      sufx
      []
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
            (* Add uncompiled header from the source tree *)
            let path =
              OASISHostPath.of_unix bs.bs_path
            in
            List.fold_left
              begin fun acc modul ->
                begin
                  try
                    [List.find
                       OASISFileUtil.file_exists_case
                       (List.map
                          (Filename.concat path)
                          (make_fnames modul [".mli"; ".ml"]))]
                  with Not_found ->
                    warning
                      (f_ "Cannot find source header for module %s \
                           in library %s")
                      modul cs.cs_name;
                    []
                end
                @
                  List.filter
                    OASISFileUtil.file_exists_case
                    (List.map
                       (Filename.concat path)
                       (make_fnames modul [".annot";".cmti";".cmt"]))
                @ acc
              end
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
    and files_of_object (f_data, acc) data_obj =
      let cs, bs, obj, obj_extra =
        !obj_hook data_obj
      in
      if var_choose bs.bs_install &&
         BaseBuilt.is_built BaseBuilt.BObj cs.cs_name then
        begin
          let acc =
            (* Start with acc + obj_extra *)
            List.rev_append obj_extra acc
          in
          let acc =
            (* Add uncompiled header from the source tree *)
            let path =
              OASISHostPath.of_unix bs.bs_path
            in
            List.fold_left
              begin fun acc modul ->
                begin
                  try
                    [List.find
                       OASISFileUtil.file_exists_case
                       (List.map
                          (Filename.concat path)
                          (make_fnames modul [".mli"; ".ml"]))]
                  with Not_found ->
                    warning
                      (f_ "Cannot find source header for module %s \
                           in object %s")
                      modul cs.cs_name;
                    []
                end
                @
                  List.filter
                    OASISFileUtil.file_exists_case
                    (List.map
                       (Filename.concat path)
                       (make_fnames modul [".annot";".cmti";".cmt"]))
                @ acc
              end
              acc
              obj.obj_modules
          in

          let acc =
            (* Get generated files *)
            BaseBuilt.fold
              BaseBuilt.BObj
              cs.cs_name
              (fun acc fn -> fn :: acc)
              acc
          in

          let f_data () =
            (* Install data associated with the object *)
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
            | Package (_, cs, bs, `Library lib, children) ->
              files_of_library data_and_files (cs, bs, lib), children
            | Package (_, cs, bs, `Object obj, children) ->
              files_of_object data_and_files (cs, bs, obj), children
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
            let _, bs, _ =
              root_lib
            in
            let res =
              Filename.concat bs.bs_path "META"
            in
            if not (OASISFileUtil.file_exists_case res) then
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
          let ocamlfind = ocamlfind () in
          let commands =
            split_install_command
              ocamlfind
              findlib_name
              meta
              files
          in
          List.iter
            (OASISExec.run ~ctxt:!BaseContext.default ocamlfind)
            commands;
          BaseLog.register install_findlib_ev findlib_name
        end;

      (* Install data files *)
      f_data ();

    in

    let group_libs, _, _ =
      findlib_mapping pkg
    in

    (* We install libraries in groups *)
    List.iter install_group_lib group_libs
  in

  let install_execs pkg =
    let install_exec data_exec =
      let cs, bs, exec =
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
                 ~tgt_fn:(cs.cs_name ^ ext_program ())
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
      let cs, doc =
        !doc_hook data
      in
      if var_choose doc.doc_install &&
         BaseBuilt.is_built BaseBuilt.BDoc cs.cs_name then
        begin
          let tgt_dir =
            OASISHostPath.of_unix (var_expand doc.doc_install_dir)
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
           if OASISFileUtil.file_exists_case data then
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
                   OASISFileUtil.rmdir ~ctxt:!BaseContext.default data
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
           OASISExec.run ~ctxt:!BaseContext.default
             (ocamlfind ()) ["remove"; data]
         end
       else
         failwithf (f_ "Unknown log event '%s'") ev;
       BaseLog.unregister ev data)
    (* We process event in reverse order *)
    (List.rev
       (BaseLog.filter
          [install_file_ev;
           install_dir_ev;
           install_findlib_ev]))


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
