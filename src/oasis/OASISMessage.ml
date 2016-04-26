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


open OASISGettext
open OASISContext


let generic_message ~ctxt lvl fmt =
  let cond =
    if ctxt.quiet then
      false
    else
      match lvl with
        | `Debug -> ctxt.debug
        | `Info  -> ctxt.info
        | _ -> true
  in
  Printf.ksprintf
    (fun str ->
       if cond then
         begin
           ctxt.printf lvl str
         end)
    fmt


let debug ~ctxt fmt =
  generic_message ~ctxt `Debug fmt


let info ~ctxt fmt =
  generic_message ~ctxt `Info fmt


let warning ~ctxt fmt =
  generic_message ~ctxt `Warning fmt


let error ~ctxt fmt =
  generic_message ~ctxt `Error fmt

