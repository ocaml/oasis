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

(** Manipulate filename
    @author Sylvain Le Gall
  *)

open Filename

module Unix = OASISUnixPath

(** Concat elements of a path
  *)
let make =
  function 
    | [] ->
        invalid_arg "BaseFilename.make"
    | hd :: tl ->
        List.fold_left Filename.concat hd tl

(** Convert a unix filename into host filename
  *)
let of_unix ufn =
  make
    (List.map
       (fun p ->
          if p = Unix.current_dir_name then
            current_dir_name
          else if p = Unix.parent_dir_name then
            parent_dir_name
          else
            p)
       (OASISUtils.split '/' ufn))

