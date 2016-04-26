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

open Format
open OASISTypes
open OASISUtils
open OASISFileTemplate
open ODN
open OASISPlugin
open BaseMessage
open OASISGettext
open OASISSetupUpdate


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


let log_change f =
  function
    | Create fn ->
      f ev_create fn

    | Change (fn, Some bak) ->
      f ev_backup (Printf.sprintf "%S -> %S" fn bak)

    | Change (fn, None) ->
      warning
        (f_ "File '%s' has no backup, won't be able to restore it.")
        fn

    | NoChange ->
      ()


(**/**)


(** Register a generated file
*)
let register =
  log_change BaseLog.register


(** Unregister a generated file
*)
let unregister =
  log_change BaseLog.unregister


let restore ?msg () =
  let msg =
    match msg with
      | Some e -> e
      | None -> !BaseContext.default
  in
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
       file_rollback ~ctxt:msg chng;
       BaseLog.unregister ev d)
    (BaseLog.filter
       [ev_create; ev_backup])


let generate ?msg
    ~restore
    ~backup
    ~setup_fn
    ?oasis_exec
    ?oasis_fn
    ?oasis_setup_args
    update
    pkg =
  let ctxt, _ =
    BaseSetup.of_package
      ?oasis_fn ?oasis_exec ?oasis_setup_args
      ~setup_update:(update = Weak) update pkg
  in

  let msg =
    match msg with
      | Some e -> e
      | None -> !BaseContext.default
  in

  let change_setup_fn =
    (* Do we need to change setup filename *)
    setup_fn <> BaseSetup.default_filename
  in

  let ctxt =
    let default_fn =
      BaseSetup.default_filename
    in
    if change_setup_fn then
      begin
        (* Copy the setup.ml file to its right filename
         * and update context accordingly
        *)
        let setup_tmpl = BaseSetup.find ctxt in

        if Sys.file_exists default_fn then
          OASISFileUtil.cp ~ctxt:msg default_fn setup_fn;
        {ctxt with
           files =
             OASISFileTemplate.add
               {setup_tmpl with fn = setup_fn}
               (OASISFileTemplate.remove
                  setup_tmpl.fn
                  ctxt.files)}
      end
    else
      ctxt
  in

  let ctxt =
    (* Fix setup for dynamic update. *)
    if update = Dynamic then
      begin
        (* We just keep setup.ml, Makefile and configure. *)
        let files =
          OASISFileTemplate.fold
            (fun tmpl acc ->
               if tmpl.fn = setup_fn then
                 OASISFileTemplate.add
                   {tmpl with body =
                                Body
                                  [
                                    if OASISFeatures.package_test
                                        OASISFeatures.dynrun_for_release pkg then
                                      BaseData.dynrun_for_release_ml
                                    else if OASISFeatures.package_test
                                        OASISFeatures.compiled_setup_ml pkg then
                                      BaseData.compiled_setup_ml
                                    else
                                      BaseData.dynrun_ml
                                  ]}
                   acc
               else if tmpl.important then
                 OASISFileTemplate.add tmpl acc
               else
                 acc)
            ctxt.files
            (OASISFileTemplate.create
               ~disable_oasis_section:pkg.OASISTypes.disable_oasis_section
               ())
        in
        {ctxt with files = files}
      end
    else
      ctxt
  in

  let () =
    if ctxt.error then
      failwith (s_ "There are errors during the file generation.")
  in

  let chngs =
    (* Generate files *)
    OASISFileTemplate.fold
      (fun tmpl acc ->
         let chng =
           try
             file_generate ~ctxt:msg ~backup tmpl
           with e ->
             List.iter (file_rollback ~ctxt:msg) acc;
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
    (* Look for the change of the setup_fn. If we change the name of setup.ml
     * we have made a copy of it and it's a creation rather than a changer.
     * So remove the backup file and change the matching file event.
    *)
    List.map
      (function
        | Change (fn, bak) as chng when setup_fn = fn ->
          begin
            let () =
              unregister chng;
              begin
                match bak with
                  | Some fn -> Sys.remove fn
                  | None    -> ()
              end
            in
            let chng =
              Create fn
            in
            if restore then
              register chng;
            chng
          end

        | e ->
          e)
      chngs

  else
    chngs
