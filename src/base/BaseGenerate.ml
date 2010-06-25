(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Generate package files
    @author Sylvain Le Gall
  *)

open Format
open OASISTypes
open OASISUtils
open OASISFileTemplate
open ODN
open OASISPlugin
open OASISMessage
open OASISGettext

let required_modules =
  [
    OASISData.oasissys_ml;
    BaseData.basesysenvironment_ml;
    (* TODO: is this module really required ? *)
    BaseData.basesys_ml;
  ]

(**/**)
let ev_create, ev_backup = 
  "restore_create", "restore_backup"
(**/**)

(** Register a generated file
  *)
let register =
  function
    | Create fn -> 
        BaseLog.register ev_create fn

    | Change (fn, Some bak) ->
        BaseLog.register ev_backup 
          (Printf.sprintf "%S -> %S" fn bak)

    | Change (fn, None) ->
        warning
          (f_ "File '%s' has no backup, won't be able to restore it.")
          fn

    | NoChange ->
        ()

(** Restore generated files, when [generate] has been called with 
    [~restore:true]
  *)
let restore () = 
  List.iter
    (fun (ev, d) ->
       let chng = 
         if ev = ev_create then
           Create d
         else if ev = ev_backup then
           Scanf.sscanf 
             d
             "%S -> %S"
             (fun fn bak -> 
                Change (fn, Some bak))
         else 
           NoChange
       in
         file_rollback chng;
         BaseLog.unregister ev d)
    (BaseLog.filter 
       [ev_create; ev_backup])


(** Generate setup file and the rest of the build system 
  *)
let generate ~restore ~backup ~dev ~setup_fn ?oasis_exec pkg = 
  let ctxt, _ = 
    BaseSetup.of_package pkg
  in

  let change_setup_fn = 
    (* Do we need to change setup_fn name *)
    setup_fn <> BaseSetup.default_fn
  in

  let ctxt =
    let default_fn =
      BaseSetup.default_fn
    in
      if change_setup_fn then
        begin
          (* Copy the setup.ml file to its right filename 
           * and update context accordingly
           *)
          let setup_tmpl = 
            BaseSetup.find ctxt
          in
            if Sys.file_exists default_fn then
              FileUtil.cp [default_fn] setup_fn;
            {ctxt with 
                 files = 
                   OASISFileTemplate.replace
                     {setup_tmpl with fn = setup_fn}
                     ctxt.files}
        end
      else
        ctxt
  in

  let ctxt = 
    (* Use BaseDev if asked to *)
    if dev then 
      fst (BaseDev.make ?oasis_exec ctxt pkg)
    else
      ctxt
  in

  let () = 
    if ctxt.error then
      exit 1
  in

  let chngs =
    (* Generate files *)
    OASISFileTemplate.fold
      (fun tmpl acc -> 
         let chng = 
           try 
             file_generate ~backup tmpl
           with e ->
             List.iter file_rollback acc;
             raise e
         in
           if restore then 
             register chng;

           chng :: acc)
      ctxt.files
      []
  in

    (* Do other actions *)
    List.iter
      (fun act -> act ())
      ctxt.other_actions;

    if change_setup_fn then
      (* Look for the change of the setup_fn.
       * If we change the name of setup.ml
       * we have made a copy of it and its
       * a creation rather than a change, also
       * remove the backup file, which are
       * not required.
       *)
      List.map
        (function
           | Change (fn, bak) when setup_fn = fn ->
               begin
                 begin 
                   match bak with
                     | Some fn -> FileUtil.rm [fn]
                     | None    -> ()
                 end;
                 Create fn
               end

           | e ->
               e)
        chngs

    else
        chngs

