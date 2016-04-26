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


(** Handle 'pager' command
*)


open BaseMessage
open OASISGettext
open OASISUtils


let pager_cmd =
  try
    Some
      (try
         Sys.getenv "OASIS_PAGER"
       with Not_found ->
         begin
           try
             Sys.getenv "PAGER"
           with Not_found ->
             OASISFileUtil.which ~ctxt:!BaseContext.default "pager"
         end)
  with Not_found ->
    None


let open_out () =
  let buf =
    Buffer.create 13
  in
  let fmt =
    Format.formatter_of_buffer buf
  in
  (buf, fmt),
  fmt


let close_out (buf, fmt) =
  let () =
    Format.pp_print_flush fmt ()
  in
  match pager_cmd with
    | Some cmd ->
      begin
        let proc =
          Unix.open_process_out cmd
        in
        let () =
          Buffer.output_buffer proc buf
        in
        match Unix.close_process_out proc with
          | Unix.WEXITED 0 ->
            ()
          | Unix.WEXITED n
          | Unix.WSIGNALED n
          | Unix.WSTOPPED n ->
            failwithf
              (f_ "Command %S exited with error code %d")
              cmd n
      end

    | None ->
      begin
        warning "%s" "Environment variable OASIS_PAGER not set.";
        Buffer.output_buffer stdout buf
      end
