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

(** Markdown formatter
    @author Sylvain Le Gall
  *)

open Format
open FormatExt

let pp_print_endblock ?check_last_char fmt () =
  match check_last_char with
    | Some s ->
        begin
          if String.length s > 0 &&
             s.[(String.length s) - 1] = '\n' then
            begin
              pp_print_newline fmt ()
            end
          else
            begin
              pp_print_newline fmt ();
              pp_print_newline fmt ()
            end
        end
    | None ->
        begin
          pp_print_newline fmt ();
          pp_print_newline fmt ()
        end

let pp_print_title lvl fmt str =
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

let pp_print_para fmt str =
  pp_open_box fmt 0;
  pp_print_string_spaced fmt str;
  pp_close_box fmt ();
  pp_print_endblock fmt ()
