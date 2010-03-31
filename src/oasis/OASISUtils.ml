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

(** Various utilities for OASIS.
  *)

module MapString = Map.Make(String)

(** Build a MapString with an association list 
  *)
let map_string_of_assoc assoc =
  List.fold_left
    (fun acc (k, v) -> MapString.add k v acc)
    MapString.empty
    assoc

(** Set for String 
  *)
module SetString = Set.Make(String)

(** Add a list to a SetString
  *)
let set_string_add_list st lst =
  List.fold_left 
    (fun acc e -> SetString.add e acc)
    st
    lst

(** Build a set out of list 
  *)
let set_string_of_list =
  set_string_add_list
    SetString.empty

(** Split a string, separator not included
  *)
let split sep str =
  let str_len =
    String.length str
  in
  let rec split_aux acc pos =
    if pos < str_len then
      (
        let pos_sep = 
          try
            String.index_from str pos sep
          with Not_found ->
            str_len
        in
        let part = 
          String.sub str pos (pos_sep - pos) 
        in
        let acc = 
          part :: acc
        in
          if pos_sep >= str_len then
            (
              (* Nothing more in the string *)
              List.rev acc
            )
          else if pos_sep = (str_len - 1) then
            (
              (* String end with a separator *)
              List.rev ("" :: acc)
            )
          else
            (
              split_aux acc (pos_sep + 1)
            )
      )
    else
      (
        List.rev acc
      )
  in
    split_aux [] 0


(** [varname_of_string ~hyphen:c s] Transform a string [s] into a variable name, 
    following this convention: no digit at the beginning, lowercase, only a-z
    and 0-9 chars. Whenever there is a problem, use an hyphen char.
  *)
let varname_of_string ?(hyphen='_') s = 
  if String.length s = 0 then
    begin
      invalid_arg "varname_of_string" 
    end
  else
    begin
      let buff = 
        Buffer.create (String.length s)
      in
        (* Start with a _ if digit *)
        if '0' <= s.[0] && s.[0] <= '9' then
          Buffer.add_char buff hyphen;

        String.iter
          (fun c ->
             if ('a' <= c && c <= 'z') 
               || 
                ('A' <= c && c <= 'Z') 
               || 
                ('0' <= c && c <= '9') then
               Buffer.add_char buff c
             else
               Buffer.add_char buff hyphen)
          s;

        String.lowercase (Buffer.contents buff)
    end

(** [varname_concat ~hyphen p s] Concat variable name, removing hyphen at end
    of [p] and at beginning of [s].
  *)
let varname_concat ?(hyphen='_') p s = 
  let p = 
    let p_len =
      String.length p
    in
      if p_len > 0 && p.[p_len - 1] = hyphen then
        String.sub p 0 (p_len - 1)
      else
        p
  in
  let s = 
    let s_len =
      String.length s
    in
      if s_len > 0 && s.[0] = hyphen then
        String.sub s 1 (s_len - 1)
      else
        s
  in
    Printf.sprintf "%s%c%s" p hyphen s


(** Fail with a format string, with 1 to 5 args.
    This is ugly but trying to use 
    Printf.ksprintf failwith, fails because the 
    return function should also be polymorphic
    but is constrained by the format types ->
    return is typed string and not 'a...
  *)
let failwithf1 fmt a =
  failwith (Printf.sprintf fmt a)

let failwithf2 fmt a b =
  failwith (Printf.sprintf fmt a b)

let failwithf3 fmt a b c =
  failwith (Printf.sprintf fmt a b c)

let failwithf4 fmt a b c d =
  failwith (Printf.sprintf fmt a b c d)

let failwithf5 fmt a b c d e =
  failwith (Printf.sprintf fmt a b c d e)

(* END EXPORT *)
