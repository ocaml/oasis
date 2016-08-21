(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


open BaseEnv
open OASISGettext
open BaseMessage
open OASISContext


let to_filename fn =
  if not (Filename.check_suffix fn ".ab") then
    warning (f_ "File '%s' doesn't have '.ab' extension") fn;
  OASISFileSystem.of_unix_filename (Filename.chop_extension fn)


let replace ~ctxt fn_lst =
  let open OASISFileSystem in
  let ibuf, obuf = Buffer.create 13, Buffer.create 13 in
  List.iter
    (fun fn ->
       Buffer.clear ibuf; Buffer.clear obuf;
       defer_close
         (ctxt.srcfs#open_in (of_unix_filename fn))
         (read_all ibuf);
       Buffer.add_string obuf (var_expand (Buffer.contents ibuf));
       defer_close
         (ctxt.srcfs#open_out (to_filename fn))
         (fun wrtr -> wrtr#output obuf))
    fn_lst
