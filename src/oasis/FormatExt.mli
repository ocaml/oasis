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

(** Extra functions for Format
    
   @author Sylvain Le Gall
  *)

(** Print a string considering ' ' as Format space.
  *)
val pp_print_string_spaced : Format.formatter -> string -> unit

(** [pp_print_list pp_elem sep fmt lst] Print the list [lst] of elements
    using [pp_elem] for each element and separate them by [sep].
  *)
val pp_print_list :
  (Format.formatter -> 'a -> unit) ->
  ('b, Format.formatter, unit) format -> Format.formatter -> 'a list -> unit
