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

type level =
  [ `Debug
  | `Info
  | `Warning
  | `Error]

type t =
  {
    quiet:                 bool;
    info:                  bool;
    debug:                 bool;
    ignore_plugins:        bool;
    ignore_unknown_fields: bool;
    printf:                level -> string -> unit;
  }

let printf lvl str =
  let beg =
    match lvl with
      | `Error -> s_ "E: "
      | `Warning -> s_ "W: "
      | `Info  -> s_ "I: "
      | `Debug -> s_ "D: "
  in
    prerr_endline (beg^str)

let default =
  ref
    {
      quiet                 = false;
      info                  = false;
      debug                 = false;
      ignore_plugins        = false;
      ignore_unknown_fields = false;
      printf                = printf;
    }

let quiet =
  {!default with quiet = true}


let args () =
  ["-quiet",
   Arg.Unit (fun () -> default := {!default with quiet = true}),
   (s_ " Run quietly");

   "-info",
   Arg.Unit (fun () -> default := {!default with info = true}),
   (s_ " Display information message");


   "-debug",
   Arg.Unit (fun () -> default := {!default with debug = true}),
   (s_ " Output debug message")]
