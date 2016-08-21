(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2011-2016, Sylvain Le Gall                                    *)
(*  Copyright (C) 2008-2011, OCamlCore SARL                                     *)
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

let w = ['0'-'9' 'a'-'z' 'A'-'Z' '_']
let s = ['\t' '\r' '\n' ' ']
let notS = _ # s
let d = ['0'-'9']

let url_scheme = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '.']*
let url_path = ( w + ':'? w* '@')? (notS+) (':' d+)?
  ('/'|'/'((w|['#' '!' ':' '.' '?' '+' '=' '&' '%' '@' '!' '-' '/'])))?

rule url = parse
  (url_scheme "://" url_path) as lxm  { lxm }

and copyright = parse
  ('(' ['c' 'C'] ')' ' '* (d+)('-' d+)? ','? ' ' _*) as lxm { lxm }

and modul = parse
  (['A'-'Z'] w*) as lxm { lxm }
