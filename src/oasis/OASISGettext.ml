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


let ns_ str =
  str


let s_ str =
  str


let f_ (str: ('a, 'b, 'c, 'd) format4) =
  str


let fn_ fmt1 fmt2 n =
  if n = 1 then
    fmt1^^""
  else
    fmt2^^""


let init =
  []


(* END EXPORT *)

(* FIXME: change this (for instance, try using dynlink?) *)

(*
IFDEF HAS_GETTEXT THEN
include
  Gettext.Library
    (struct
      let textdomain   = "oasis"
      let codeset      = None
      let dir          = None
      let dependencies = Gettext.init
    end)
ENDIF
   *)
