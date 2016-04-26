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


(** Dump function calls with ODN

    The idea of this module is to store a function and its argument with the ODN
    data structure that should be dumped. This allows to type in a way the
    function to be dumped.

    This module is {b not exported}.

    @see <http://forge.ocamlcore.org/projects/odn> OCaml Data Notation project
    @author Sylvain Le Gall
*)


(** Function that can be generated using ODN
    func_call = APP(func, [], [func_arg]).
*)
type 'a func =
  {
    func_call: 'a;
    func_name: string;
    func_arg:  ODN.t option;
  }


(** Return the OCaml function corresponding to a [func].
*)
val func: 'a -> string -> 'a func


(** Create a func with an argument
*)
val func_with_arg: ('a -> 'b) -> string -> 'a -> ('a -> ODN.t) -> 'b func


(** Return the [ODN.t] code corresponding to a [func].
*)
val odn_of_func: 'a func -> ODN.t


(** Return the OCaml function corresponding to a [func].
*)
val func_call: 'a func -> 'a
