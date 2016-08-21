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

type 'a filename

val of_unix_filename: OASISUnixPath.unix_filename -> 'a filename
val to_unix_filename: 'a filename -> OASISUnixPath.unix_filename

class type closer =
  object
    method close: unit
  end

class type reader =
  object
    inherit closer
    method input: Buffer.t -> int -> unit
  end

class type writer =
  object
    inherit closer
    method output: Buffer.t -> unit
  end

class type ['a] fs =
  object
    (** Return a string representation of the filename. It is may not be a real
        host filename.
    *)
    method string_of_filename: 'a filename -> string

    method open_out:
      ?mode:(open_flag list) -> ?perm:int -> 'a filename -> writer

    method open_in:
      ?mode:(open_flag list) -> ?perm:int -> 'a filename -> reader

    method file_exists: 'a filename -> bool

    method remove: 'a filename -> unit
  end

val defer_close: (#closer as 'a) -> ('a -> 'b) -> 'b

val binary_out: open_flag list
val binary_in: open_flag list

val stream_of_reader: #reader -> char Stream.t

val read_all: Buffer.t -> #reader -> unit

class ['a] host_fs: OASISUnixPath.host_filename -> ['a] fs
