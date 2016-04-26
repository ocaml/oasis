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


open BaseEnv
open OASISGettext
open BaseMessage


let to_filename fn =
  let fn =
    OASISHostPath.of_unix fn
  in
  if not (Filename.check_suffix fn ".ab") then
    warning
      (f_ "File '%s' doesn't have '.ab' extension")
      fn;
  Filename.chop_extension fn


let replace fn_lst =
  let buff =
    Buffer.create 13
  in
  List.iter
    (fun fn ->
       let fn =
         OASISHostPath.of_unix fn
       in
       let chn_in =
         open_in fn
       in
       let chn_out =
         open_out (to_filename fn)
       in
       (
         try
           while true do
             Buffer.add_string buff (var_expand (input_line chn_in));
             Buffer.add_char buff '\n'
           done
         with End_of_file ->
           ()
       );
       Buffer.output_buffer chn_out buff;
       Buffer.clear buff;
       close_in chn_in;
       close_out chn_out)
    fn_lst
