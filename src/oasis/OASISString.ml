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


(** Various string utilities.

    Mostly inspired by extlib and batteries ExtString and BatString libraries.

    @author Sylvain Le Gall
*)


let nsplitf str f =
  if str = "" then
    []
  else
    let buf = Buffer.create 13 in
    let lst = ref [] in
    let push () =
      lst := Buffer.contents buf :: !lst;
      Buffer.clear buf
    in
    let str_len = String.length str in
    for i = 0 to str_len - 1 do
      if f str.[i] then
        push ()
      else
        Buffer.add_char buf str.[i]
    done;
    push ();
    List.rev !lst


(** [nsplit c s] Split the string [s] at char [c]. It doesn't include the
    separator.
*)
let nsplit str c =
  nsplitf str ((=) c)


let find ~what ?(offset=0) str =
  let what_idx = ref 0 in
  let str_idx = ref offset in
  while !str_idx < String.length str &&
        !what_idx < String.length what do
    if str.[!str_idx] = what.[!what_idx] then
      incr what_idx
    else
      what_idx := 0;
    incr str_idx
  done;
  if !what_idx <> String.length what then
    raise Not_found
  else
    !str_idx - !what_idx


let sub_start str len =
  let str_len = String.length str in
  if len >= str_len then
    ""
  else
    String.sub str len (str_len - len)


let sub_end ?(offset=0) str len =
  let str_len = String.length str in
  if len >= str_len then
    ""
  else
    String.sub str 0 (str_len - len)


let starts_with ~what ?(offset=0) str =
  let what_idx = ref 0 in
  let str_idx = ref offset in
  let ok = ref true in
  while !ok &&
        !str_idx < String.length str &&
        !what_idx < String.length what do
    if str.[!str_idx] = what.[!what_idx] then
      incr what_idx
    else
      ok := false;
    incr str_idx
  done;
  if !what_idx = String.length what then
    true
  else
    false


let strip_starts_with ~what str =
  if starts_with ~what str then
    sub_start str (String.length what)
  else
    raise Not_found


let ends_with ~what ?(offset=0) str =
  let what_idx = ref ((String.length what) - 1) in
  let str_idx = ref ((String.length str) - 1) in
  let ok = ref true in
  while !ok &&
        offset <= !str_idx &&
        0 <= !what_idx do
    if str.[!str_idx] = what.[!what_idx] then
      decr what_idx
    else
      ok := false;
    decr str_idx
  done;
  if !what_idx = -1 then
    true
  else
    false


let strip_ends_with ~what str =
  if ends_with ~what str then
    sub_end str (String.length what)
  else
    raise Not_found


let replace_chars f s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_char buf (f c)) s;
  Buffer.contents buf


(* END EXPORT *)


let is_whitespace =
  function
    | ' ' | '\r' | '\n' | '\t' -> true
    |  _  -> false


let tokenize ?(is_whitespace=is_whitespace) ?(tokens=[]) str =
  let lst = ref [] in
  let buf = Buffer.create 13 in
  let idx = ref 0 in
  let push () =
    (* Push the content of the buffer on the list. *)
    if Buffer.length buf > 0 then
      begin
        lst := Buffer.contents buf :: !lst;
        Buffer.clear buf
      end
  in
  let match_token () =
    List.exists
      (fun tok ->
         if starts_with ~what:tok ~offset:!idx str then
           begin
             push ();
             lst := tok :: !lst;
             idx := !idx + (String.length tok);
             true
           end
         else
           false)
      tokens
  in
  while !idx < String.length str do
    let c = str.[!idx] in
    if is_whitespace c then
      begin
        push ();
        incr idx
      end
    else if match_token () then
      begin
        ()
      end
    else
      begin
        Buffer.add_char buf c;
        incr idx
      end
  done;
  push ();
  List.rev !lst


let tokenize_genlex ?(tokens=[]) str =
  let strm = Genlex.make_lexer tokens (Stream.of_string str) in
  let lst = ref [] in
  Stream.iter (fun tok -> lst := tok :: !lst) strm;
  List.rev !lst


let split str c =
  let idx = String.index str c in
  String.sub str 0 idx,
  String.sub str (idx + 1) (String.length str - idx - 1)


let trim str =
  let start_non_blank = ref 0 in
  let stop_non_blank = ref ((String.length str) - 1) in
  while !start_non_blank < String.length str &&
        is_whitespace (str.[!start_non_blank]) do
    incr start_non_blank
  done;
  while !start_non_blank <= !stop_non_blank &&
        is_whitespace (str.[!stop_non_blank]) do
    decr stop_non_blank
  done;
  String.sub str !start_non_blank (!stop_non_blank - !start_non_blank + 1)


let fold_left f acc str =
  let racc = ref acc in
  for i = 0 to String.length str - 1 do
    racc := f !racc str.[i]
  done;
  !racc


let contains ~what str =
  (* Implementation is naive. *)
  let len_what = String.length what in
  let len_str = String.length str in
  let rec check idx_what idx_str =
    if idx_what >= len_what then
      true
    else if idx_str >= len_str then
      false
    else if str.[idx_str] = what.[idx_what] then
      check (idx_what + 1) (idx_str + 1)
    else
      check 0 (idx_str + 1)
  in
  check 0 0


(** Split a list using ',' as separator. {b Not exported} *)
let split_comma str =
  List.map trim (nsplit str ',')


(** Split a list using '\n' as separator. {b Not exported} *)
let split_newline ?(do_trim=true) str =
  let lst = nsplit str '\n' in
  if do_trim then
    List.map trim lst
  else
    lst


(** Split a string containing '(...)' optionally. {b Not exported} *)
let split_optional_parentheses str =
  try
    let beg_str, end_str = split (trim str) '(' in
    let content_str = strip_ends_with ~what:")" end_str in
    trim beg_str,
    Some (trim content_str)
  with Not_found ->
    trim str, None
