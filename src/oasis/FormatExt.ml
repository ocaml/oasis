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
