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


(** Common arguments
*)


open OASISGettext


let define_oasis_fn f () =
  let oasis_fn = ref OASISParse.default_oasis_fn in
  let specs' =
    "-oasis",
    Arg.Set_string oasis_fn,
    s_ "fn _oasis file to use.";
  in
  let wrap g ~ctxt =
    g ~ctxt !oasis_fn
  in
  let (specs, anon), run = f () in
  (specs' :: specs, anon), wrap run


let parse_oasis_fn f =
  define_oasis_fn
    (fun () ->
       let wrap g ~ctxt fn =
         let pkg = OASISParse.from_file ~ctxt fn in
         g ~ctxt fn pkg
       in
       let (specs, anon), run = f () in
       (specs, anon), wrap run)
