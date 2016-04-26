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


(** Manipulate host filenames
    @author Sylvain Le Gall
*)


open OASISTypes


(** Create a filename out of its components.
*)
val make: host_filename list -> host_filename


(** Convert a unix filename into host filename.
*)
val of_unix: unix_filename -> host_filename


(** Convert a host filename into a unix filename.
    {b Not exported}
*)
val to_unix: host_filename -> unix_filename


(** Compare host filename.
    {b Not exported}
*)
val compare: host_filename -> host_filename -> int


(** See {!OASISUnixPath.add_extension}.
    {b Not exported}
*)
val add_extension: host_filename -> string -> host_filename


(** Map for host filename. {b Not exported.} *)
module Map: OASISUtils.MapExt.S with type key = host_filename
