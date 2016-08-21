(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


open OASISGettext


let default_oasis_fn = "_oasis"

let critical = !OASISMutex.make ()

let parse ~ctxt ?fn lexbuf =
  let open Lexing in
  let pos_fname =
    match fn with
    | Some fn -> fn
    | None -> s_ "<undefined>"
  in
  let ast =
    lexbuf.lex_curr_p <-
      {pos_fname = pos_fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
    OASISMutex.section critical
      (fun () -> OASISAst_parser.main (OASISAst_lexer.token ~ctxt ()) lexbuf)
  in
    OASISAst.to_package ~ctxt ast


let from_stream ~ctxt ?fn st =
  let str =
    let buf = Buffer.create 13 in
    Stream.iter (Buffer.add_char buf) st;
    Buffer.contents buf
  in
  parse ~ctxt ?fn (Lexing.from_string str)


let from_file ~ctxt fn =
  let chn = open_in fn in
  let close () = close_in chn in
  try
    let pkg = parse ~ctxt ~fn (Lexing.from_channel chn) in
    close ();
    pkg
  with e ->
    close ();
    raise e


let from_string ~ctxt ?fn str = parse ~ctxt ?fn (Lexing.from_string str)
