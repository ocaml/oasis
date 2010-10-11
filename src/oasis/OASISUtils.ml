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

module MapString = Map.Make(String)

let map_string_of_assoc assoc =
  List.fold_left
    (fun acc (k, v) -> MapString.add k v acc)
    MapString.empty
    assoc

module SetString = Set.Make(String)

let set_string_add_list st lst =
  List.fold_left 
    (fun acc e -> SetString.add e acc)
    st
    lst

let set_string_of_list =
  set_string_add_list
    SetString.empty


let compare_csl s1 s2 = 
  String.compare (String.lowercase s1) (String.lowercase s2)

module HashStringCsl = 
  Hashtbl.Make
    (struct
       type t = string

       let equal s1 s2 = 
           (String.lowercase s1) = (String.lowercase s2)

       let hash s =
         Hashtbl.hash (String.lowercase s)
     end)

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


let is_varname str = 
  str = varname_of_string str

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

open ExtString

let split_comma str = 
  List.map String.strip (String.nsplit str ",")

let split_optional_parentheses = 
  let split_parentheses =
    ignore "(*(*";
    Pcre.regexp "([^\\(]*)\\(([^\\)]*)\\)"
  in
    fun str -> 
      try 
        let substrs =
          Pcre.exec ~rex:split_parentheses str
        in
        let s1, s2 = 
          Pcre.get_substring substrs 1,
          Pcre.get_substring substrs 2
        in
        let e1 = 
          String.strip s1
        in
        let e2 =
          String.strip s2
        in
          e1, Some e2
      with Not_found ->
        String.strip str, None
