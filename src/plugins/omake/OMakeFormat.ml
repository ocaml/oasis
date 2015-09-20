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


(** Write omake files
    @author Gerd Stolpmann
  *)

open Printf

type om_entry =
  | Section of om_entry list
  | Set_string of om_append * string * om_value
  | Set_array of om_append * string * om_value list
  | Export of string list
  | Lines of string list
  | Nop
      
 and om_append = bool
                   
 and om_value =
   | Literal of string
   | Expression of string
   | Concat of om_value list

let write_const_file ?(skip_existing=false) upath data =
  let hpath = OASISHostPath.of_unix upath in
  if not skip_existing || not (Sys.file_exists hpath) then (
    let f = open_out hpath in
    output_string f data;
    close_out f
  )

let rec string_of_value =
  function
  | Literal s ->
      let b = Buffer.create 80 in
      Buffer.add_string b "$\"";
      String.iter
        (function
          | '"'  -> Buffer.add_string b "$(string $'\"')"
          | '$'  -> Buffer.add_string b "$(string $'$')"
          | c    -> Buffer.add_char b c
        )
        s;
      Buffer.add_string b "\"";
      Buffer.contents b
  | Expression s ->
      s
  | Concat l ->
      String.concat "" (List.map string_of_value l)


let write_omake_file ?(skip_existing=false) upath entries =
  let write_indent f indent =
    output_string f (String.make indent ' ') in
  let rec write f indent entry =
    match entry with
      | Section l ->
          write_indent f indent;
          fprintf f "section\n";
          List.iter (write f (indent+4)) l
      | Set_string(append,name,value) ->
          write_indent f indent;
          fprintf f "%s %s= %s\n"
                  name (if append then "+" else "") (string_of_value value)
      | Set_array(append,name,values) ->
          write_indent f indent;
          fprintf f "%s[] %s=\n" name (if append then "+" else "");
          List.iter
            (fun value ->
               write_indent f (indent+4);
               fprintf f "%s\n" (string_of_value value)
            )
            values
      | Export names ->
          write_indent f indent;
          fprintf f "export %s\n" (String.concat " " names)
      | Lines lines ->
          List.iter
            (fun line ->
               write_indent f indent;
               fprintf f "%s\n" line
            )
            lines
      | Nop ->
          ()
  in
  let hpath = OASISHostPath.of_unix upath in
  if not skip_existing || not (Sys.file_exists hpath) then (
    let f = open_out hpath in
    List.iter (write f 0) entries;
    close_out f
  )


(* END EXPORT *)
