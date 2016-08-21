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

{
open Query_parser
}

rule token = parse
  [' ''\t''\n']  { token lexbuf }
| ['A'-'Z' 'a'-'z' '0'-'9' '_']+ as lxm { ID lxm }
| '"'            { STR (str (Buffer.create 13) lexbuf) }
| '('            { LPAREN }
| ')'            { RPAREN }
| '.'            { DOT }
| eof            { EOF }

and str buf = parse
  '\\' '"' {Buffer.add_char buf '"'; str buf lexbuf}
| '"'      {Buffer.contents buf}
| _ as c   {Buffer.add_char buf c; str buf lexbuf}
