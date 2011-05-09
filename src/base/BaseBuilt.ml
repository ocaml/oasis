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

open OASISTypes
open OASISGettext
open BaseStandardVar
open BaseMessage

type t =
  | BExec    (* Executable *)
  | BExecLib (* Library coming with executable *)
  | BLib     (* Library *)
  | BDoc     (* Document *)

let to_log_event_file t nm =
  "built_"^
  (match t with
     | BExec -> "exec"
     | BExecLib -> "exec_lib"
     | BLib -> "lib"
     | BDoc -> "doc")^
  "_"^nm

let to_log_event_done t nm =
  "is_"^(to_log_event_file t nm)

let register t nm lst =
  BaseLog.register
    (to_log_event_done t nm)
    "true";
  List.iter
    (fun alt ->
       let registered =
         List.fold_left
           (fun registered fn ->
              if Sys.file_exists fn then
                begin
                  BaseLog.register
                    (to_log_event_file t nm)
                    (if Filename.is_relative fn then
                       Filename.concat (Sys.getcwd ()) fn
                     else
                       fn);
                  true
                end
              else
                registered)
           false
           alt
       in
         if not registered then
           warning
             (f_ "Cannot find an existing alternative files among: %s")
             (String.concat (s_ ", ") alt))
    lst

let unregister t nm =
  List.iter
    (fun (e, d) ->
       BaseLog.unregister e d)
    (BaseLog.filter
       [to_log_event_file t nm;
        to_log_event_done t nm])

let fold t nm f acc =
  List.fold_left
    (fun acc (_, fn) ->
       if Sys.file_exists fn then
         begin
           f acc fn
         end
       else
         begin
           warning
             (f_ "File '%s' has been marked as built \
                for %s but doesn't exist")
             fn
             (Printf.sprintf
                (match t with
                   | BExec | BExecLib ->
                       (f_ "executable %s")
                   | BLib ->
                       (f_ "library %s")
                   | BDoc ->
                       (f_ "documentation %s"))
                nm);
           acc
         end)
    acc
    (BaseLog.filter
       [to_log_event_file t nm])

let is_built t nm =
  List.fold_left
    (fun is_built (_, d) ->
       (try
          bool_of_string d
        with _ ->
          false))
    false
    (BaseLog.filter
       [to_log_event_done t nm])

let of_executable ffn (cs, bs, exec) =
  let unix_exec_is, unix_dll_opt =
    OASISExecutable.unix_exec_is
      (cs, bs, exec)
      (fun () ->
         bool_of_string
           (is_native ()))
      ext_dll
      ext_program
  in
  let evs =
    (BExec, cs.cs_name, [[ffn unix_exec_is]])
    ::
    (match unix_dll_opt with
       | Some fn ->
           [BExecLib, cs.cs_name, [[ffn fn]]]
       | None ->
           [])
  in
    evs,
    unix_exec_is,
    unix_dll_opt

let of_library ffn (cs, bs, lib) =
  let unix_lst =
    OASISLibrary.generated_unix_files
      ~ctxt:!BaseContext.default
      (cs, bs, lib)
      (fun fn ->
         Sys.file_exists (BaseFilePath.of_unix fn))
      (fun () ->
         bool_of_string (is_native ()))
      ext_lib
      ext_dll
  in
  let evs =
    [BLib,
     cs.cs_name,
     List.map (List.map ffn) unix_lst]
  in
    evs, unix_lst

