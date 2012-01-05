(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

let failwithf fmt = Printf.ksprintf failwith fmt

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

module POSIX =
struct
  (* If a backslash appears at the end of the substring [s.[i0
     .. i1-1]], it will be removed (it can be thought as a backslash
     to continue the line but lines are read one by one). *)
  let remove_backslashes s i0 i1 ~bs =
    assert(i0 < i1);
    if bs <= 0 then String.sub s i0 (i1 - i0)
    else
      let len = i1 - i0 - bs in
      let s' = String.create len in
      let i = ref i0 in
      let j = ref 0 in
      while !i < i1 do
        if s.[!i] <> '\\' then (s'.[!j] <- s.[!i]; incr j);
        incr i
      done;
      s'

  let unescape s =
    let len = String.length s in
    let bs = ref 0 in
    let i = ref 0 in
    while !i < len do
      if s.[!i] = '\\' then (
        incr bs; (* will need to remove '\\' *)
        incr i; (* skip next char *)
      );
      incr i
    done;
    remove_backslashes s 0 len ~bs:!bs

  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  (* [escape_if_spaces s] escapes [s] if it contains spaces in such a way
     that [unescape] recovers the original string. *)
  let escape s =
    let has_space = ref false
    and nquotes = ref 0 in
    for i = 0 to String.length s - 1 do
      if is_space s.[i] then has_space := true
      else if s.[i] = '"' then incr nquotes
    done;
    if !has_space then
      if !nquotes = 0 then "\"" ^ s ^ "\""
      else
        let len' = String.length s + !nquotes + 2 in
        let s' = String.create len' in
        s'.[0] <- '"';
        let j = ref 1 in
        for i = 0 to String.length s - 1 do
          if s.[i] = '"' then (s'.[!j] <- '\\'; incr j);
          s'.[!j] <- s.[i];
          incr j
        done;
        s'.[!j] <- '"';
        s'
    else s

  (* Return the first index [j >= i] such that [s.[j]] is not a space
     or [i = i1] if no such [j] exists. *)
  let rec skip_spaces s i i1 =
    if i >= i1 then i1
    else if is_space s.[i] then skip_spaces s (i + 1) i1
    else i

  let add_remove_backslashes s i0 i1 ~bs hunks =
    if i0 >= i1 then hunks
    else remove_backslashes s i0 i1 ~bs :: hunks

  (* Parts of strings may be gathered differently (e.g. a string can
     be _partly_ quoted).  This will gather the various pieces. *)
  let concat_hunks h = String.concat "" (List.rev h)

  (* FIXME: Not handled (does it make sense in this context?)
     • $'string'
     • special parameters: @, *,...    *)
  let rec split s =
    let len = String.length s in
    let i = skip_spaces s 0 len in
    get_split_string [] s i i len ~bs:0
  and get_split_string hunks s i0 i i1 ~bs =
    (* [i0 <= i < i1].  [~bs] is the number of backslahes to remove in
       the current substring.  [hunks] is the list of pieces of the
       current argument that were previously gathered. *)
    if i >= i1 then
      if i0 >= i1 then
        if hunks = [] then [] else [concat_hunks hunks]
      else [concat_hunks(add_remove_backslashes s i0 i1 ~bs hunks)]
    else if s.[i] = '\\' then
      (* Treat the next char as a std one (even if '$',...). *)
      get_split_string hunks s i0 (i + 2) i1 ~bs:(bs + 1)
    else if is_space s.[i] then
      let hunks = add_remove_backslashes s i0 i ~bs hunks in
      let i = skip_spaces s (i + 1) i1 in
      concat_hunks hunks :: get_split_string [] s i i i1 ~bs:0
    else if s.[i] = '\'' then
      let hunks = add_remove_backslashes s i0 i ~bs hunks in
      simply_quoted hunks s (i + 1) (i + 1) i1
    else if s.[i] = '"' then
      let hunks = add_remove_backslashes s i0 i ~bs hunks in
      doubly_quoted hunks s (i + 1) (i + 1) i1 ~bs:0
    else if s.[i] = '$' then
      if i + 1 >= i1 then (* final '$' *)
        [concat_hunks(add_remove_backslashes s i0 i1 ~bs hunks)]
      else
        let hunks = add_remove_backslashes s i0 i ~bs hunks in
        let v, inext = get_dollar_var s (i + 1) i1 in
        get_split_string (v :: hunks) s inext inext i1 ~bs:0
    else get_split_string hunks s i0 (i + 1) i1 ~bs

  and simply_quoted hunks s i0 i i1 =
    if i >= i1 then failwithf (f_ "Unterminated simply quoted string in %S") s
    else if s.[i] = '\'' then
      let v = String.sub s i0 (i - i0) (* no escape *) in
      get_split_string (v :: hunks) s (i + 1) (i + 1) i1 ~bs:0
    else simply_quoted hunks s i0 (i + 1) i1

  and doubly_quoted hunks s i0 i i1 ~bs =
    if i >= i1 then failwithf(f_ "Unterminated doubly quoted string in %S") s
    else if s.[i] = '\\' then
      (* Technically we wan only escape '$' '`' '"' '\\' and <newline>.
         FIXME: Do we want to impose this? *)
      doubly_quoted hunks s i0 (i + 2) i1 ~bs:(bs + 1)
    else if s.[i] = '"' then
      let h = remove_backslashes s i0 i ~bs in
      get_split_string (h :: hunks) s (i + 1) (i + 1) i1 ~bs:0
    else if s.[i] = '$' then
      let hunks = add_remove_backslashes s i0 i ~bs hunks in
      let v, inext = get_dollar_var s (i + 1) i1 in
      doubly_quoted (v :: hunks) s inext inext i1 ~bs:0
    else doubly_quoted hunks s i0 (i + 1) i1 ~bs

  and get_dollar_var s i0 i1 =
    (* Whether they occur or not in a doubly quoted string, quotes
       between $(...) are treated like if they are in an unquoted
       environment (thus with no need of escaping). *)
    if i0 >= i1 then failwithf (f_ "$-expansion not terminated in %S") s
    else if s.[i0] = '(' then parse_dollar_var s (i0 + 1) (i0 + 1) i1 ')'
    else if s.[i0] = '{' then parse_dollar_var s (i0 + 1) (i0 + 1) i1 '}'
    else failwithf (f_ "$-expansion is neither $(...) nor ${...} in %S") s
  and parse_dollar_var s i0 i i1 closing =
    (* FIXME: very naive; does not handle [a"b$(echo c "d)")"] *)
    if i >= i1 then
      failwithf (f_ "Missing '%c' closing $-expansion in %S") closing s
    else if s.[i] = closing then
      ("$(" ^ String.sub s i0 (i - i0)  ^ ")", i + 1)
    else parse_dollar_var s i0 (i + 1) i1 closing
end
