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


let split_comma str =
  List.map BatString.strip (BatString.nsplit str ",")

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
          BatString.strip s1
        in
        let e2 =
          BatString.strip s2
        in
          e1, Some e2
      with Not_found ->
        BatString.strip str, None

module POSIXShell =
struct

  let unescape s =
    let buf = Buffer.create (String.length s) in
    ignore(BatString.fold_left
             (fun escaped_char ->
              function
              | '\\' when not escaped_char ->
                true (* Next char should be added anyway *)
              | c ->
                Buffer.add_char buf c;
                false)
             false s);
    Buffer.contents buf

  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  (* [escape s] escapes [s] in such a way that [unescape] recovers the
     original string. *)
  let escape s =
    let buf = Buffer.create (String.length s) in
    let need_to_quote =
      BatString.fold_left
        (fun need_to_quote ->
         function
         | '"' ->
           Buffer.add_string buf "\\\"";
           true
         | c ->
           Buffer.add_char buf c;
           need_to_quote || is_space c)
        false
        s
    in
    if need_to_quote then "\"" ^ (Buffer.contents buf) ^ "\""
    else s

  (* FIXME: Not handled (does it make sense in this context?)
     • $'string'
   *)
  let rec split str =
    (* Buffer holding the current arg being parsed. *)
    let buf = Buffer.create (String.length str) in

    (* Shorthands to access the buffer *)
    let buf_add c =
      Buffer.add_char buf c
    in
    let buf_flush () =
      let res = Buffer.contents buf in
      Buffer.clear buf;
      res
    in

    (* Protect Buffer.add_substitute substitution inside a string, the $... will
     * be transformed into $X0, $X1,...
     *)
    let substr_data =
      Hashtbl.create 13
    in

    let str =
      let idx = ref 0 in
      try
        Buffer.add_substitute
          buf
          (fun var ->
             let nvar = Printf.sprintf "X%d" !idx in
               incr idx;
               Hashtbl.add substr_data nvar var;
               "${" ^ nvar ^ "}")
          str;
        buf_flush ()
      with Not_found ->
        failwithf
          (f_ "Unterminated substitution $(...) or ${...} in the string %S")
          str
    in

    (* Function to unprotect a string protected above. *)
    let unprotect_subst str =
      let add_end_dollar, str =
        let len = String.length str in
          if len > 0 && str.[len - 1] = '$' then
            true, String.sub str 0 (len - 1)
          else
            false, str
      in
        Buffer.add_substitute
          buf
          (fun nvar ->
           let var = try Hashtbl.find substr_data nvar
                     with Not_found -> nvar in
           (* The protection, using [Buffer.add_substitute], ensures that
              if [var] contains '}' then it was delimited with '(', ')'. *)
           if String.contains var '}' then "$("^var^")"
           else "${"^var^"}"
          )
          str;
        if add_end_dollar then
          buf_add '$';
        buf_flush ()
    in

    let rec skip_blank strm =
      match Stream.peek strm with
        | Some c ->
            if is_space c then
              begin
                Stream.junk strm;
                skip_blank strm
              end
        | None ->
            ()
    in

    let rec get_simply_quoted_string strm =
      try
        match Stream.next strm with
          | '\'' ->
              (* End of simply quoted string *)
              ()
          | c ->
              buf_add c;
              get_simply_quoted_string strm

      with Stream.Failure ->
        failwithf
          (f_ "Unterminated simply quoted string in %S")
          (unprotect_subst (buf_flush ()))
    in

    let is_doubly_quoted_escapable =
      function
        | Some c ->
            c = '$' || c = '`' || c = '"' || c = '\\'
        | None ->
            false
    in

    let get_escape_char strm =
      match Stream.peek strm with
        | Some c ->
            buf_add c;
            Stream.junk strm
        | None ->
            (* Final backslash, ignore it *)
            ()
    in

    let rec get_doubly_quoted_string strm =
      try
        match Stream.next strm with
          | '"' ->
              (* End of doubly quoted string *)
              ()
          | '\\' when is_doubly_quoted_escapable (Stream.peek strm)  ->
              get_escape_char strm;
              get_doubly_quoted_string strm
          | c ->
              buf_add c;
              get_doubly_quoted_string strm

      with Stream.Failure ->
        failwithf
          (f_ "Unterminated doubly quoted string in %S")
          (unprotect_subst (buf_flush ()))
    in

    (* The char stream used for parsing *)
    let strm =
      Stream.of_string str
    in

    let rargs =
      ref []
    in

    let () =
      (* Skip blanks at the beginning *)
      skip_blank strm;
      while Stream.peek strm <> None do
        match Stream.next strm with
          | '\\' ->
              (* Escape a char, since it is possible that get_escape_char
               * decide to ignore the '\\', we let this function choose to
               * add or not '\\'.
               *)
              get_escape_char strm

          | '\'' ->
              get_simply_quoted_string strm

          | '"' ->
              get_doubly_quoted_string strm

          | c ->
              if is_space c then
                begin
                  (* We reach the end of an arg *)
                  rargs := buf_flush () :: !rargs;
                  skip_blank strm
                end
              else
                begin
                  buf_add c
                end
      done;
      let last = buf_flush () in
      if last <> "" then rargs := last :: !rargs
    in
    List.rev_map unprotect_subst !rargs

end
