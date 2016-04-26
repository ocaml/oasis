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


open OASISTypes


let unix_exec_is (cs, bs, exec) is_native ext_dll suffix_program =
  let dir =
    OASISUnixPath.concat
      bs.bs_path
      (OASISUnixPath.dirname exec.exec_main_is)
  in
  let is_native_exec =
    match bs.bs_compiled_object with
      | Native -> true
      | Best -> is_native ()
      | Byte -> false
  in

  OASISUnixPath.concat
    dir
    (cs.cs_name^(suffix_program ())),

  if not is_native_exec &&
     not exec.exec_custom &&
     bs.bs_c_sources <> [] then
    Some (dir^"/dll"^cs.cs_name^"_stubs"^(ext_dll ()))
  else
    None


(* END EXPORT *)


let schema = OASISExecutable_intern.schema
