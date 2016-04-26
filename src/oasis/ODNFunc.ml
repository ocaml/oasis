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


type 'a func =
  {
    func_call: 'a;
    func_name: string;
    func_arg:  ODN.t option;
  }


let func f f_nm =
  {
    func_call = f;
    func_name = f_nm;
    func_arg  = None;
  }


let func_with_arg f f_nm arg odn_of_arg =
  {
    func_call = f arg;
    func_name = f_nm;
    func_arg  = Some (odn_of_arg arg);
  }


let odn_of_func t =
  match t.func_arg with
    | Some arg ->
      ODN.APP (t.func_name, [], [arg])
    | None ->
      ODN.VAR t.func_name


let func_call t =
  t.func_call
