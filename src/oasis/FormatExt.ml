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


open Format


let pp_print_string_spaced fmt str =
  String.iter
    (function
      | ' ' -> Format.pp_print_space fmt ()
      | '\n' -> Format.pp_print_space fmt ()
      | c -> Format.pp_print_char fmt c)
    str


let pp_print_list pp_elem lst_sep fmt =
  function
    | [] ->
      ()
    | hd :: tl ->
      pp_elem fmt hd;
      List.iter
        (fun e ->
           fprintf fmt lst_sep;
           pp_elem fmt e)
        tl


let pp_print_cut2 fmt () =
  pp_print_cut fmt ();
  pp_print_cut fmt ()


let pp_print_endblock ?(check_last_char="") fmt () =
  if OASISString.ends_with ~what:"\n" check_last_char then begin
    pp_print_newline fmt ()
  end else begin
    pp_print_newline fmt ();
    pp_print_newline fmt ()
  end


let pp_print_para fmt ?(end_para=true) str =
  let str_len =
    String.length str
  in
  let rec decode_string i =
    if i < str_len then
      begin
        match str.[i] with
          | ' ' ->
            pp_print_space fmt ();
            decode_string (i + 1)

          | '\n' ->
            if i + 1 < str_len && str.[i + 1] = '\n' then
              begin
                pp_close_box fmt ();
                pp_print_cut2 fmt ();
                pp_open_box fmt 0;
                decode_string (i + 2)
              end
            else
              begin
                pp_print_space fmt ();
                decode_string (i + 1)
              end

          | c ->
            pp_print_char fmt c;
            decode_string (i + 1)
      end;
  in
  pp_open_vbox fmt 0;
  pp_open_box fmt 0;
  decode_string 0;
  pp_close_box fmt ();
  if end_para then
    pp_print_cut2 fmt ();
  pp_close_box fmt ()


let pp_print_paraf fmt ?end_para fmt' =
  Printf.ksprintf (pp_print_para fmt ?end_para) fmt'


let pp_print_title fmt lvl str =
  let pp_print_underlined c fmt str =
    pp_print_string fmt str;
    pp_print_newline fmt ();
    pp_print_string fmt (String.make (String.length str) c)
  in
  if lvl = 1 then
    pp_print_underlined '=' fmt str
  else if lvl = 2 then
    pp_print_underlined '-' fmt str
  else
    begin
      (* ATX style *)
      pp_print_string fmt (String.make lvl '#');
      pp_print_string fmt str
    end;
  pp_print_endblock fmt ()


let pp_print_titlef fmt lvl fmt' =
  Printf.ksprintf (pp_print_title fmt lvl) fmt'


let pp_print_def fmt term defs =
  pp_print_string fmt term;
  pp_print_newline fmt ();
  List.iter
    (fun (pp_print_e, e) ->
       pp_print_string fmt ":   ";
       pp_open_box fmt 0;
       pp_print_e fmt e;
       pp_close_box fmt ();
       pp_print_newline fmt ())
    defs;
  pp_print_newline fmt ()

