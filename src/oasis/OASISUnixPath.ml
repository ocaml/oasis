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

type unix_filename = string

let current_dir_name = "."

let parent_dir_name = ".."

let concat f1 f2 = 
  if f1 = current_dir_name then
    f2
  else if f2 = current_dir_name then
    f1
  else
    f1^"/"^f2

let make =
  function
    | hd :: tl ->
        List.fold_left
          (fun f p -> concat f p)
          hd
          tl
    | [] ->
        invalid_arg "OASISUnixPath.make"

let dirname f =
  try
    String.sub f 0 (String.rindex f '/')
  with Not_found ->
    current_dir_name

let basename f =
  try 
    let pos_start =
      (String.rindex f '/') + 1
    in
      String.sub f pos_start ((String.length f) - pos_start)
  with Not_found ->
    f

let chop_extension f =
  try 
    let last_dot =
      String.rindex f '.'
    in
    let sub =
      String.sub f 0 last_dot
    in
      try 
        let last_slash =
          String.rindex f '/'
        in
          if last_slash < last_dot then
            sub
          else
            f
      with Not_found ->
        sub

  with Not_found ->
    f

