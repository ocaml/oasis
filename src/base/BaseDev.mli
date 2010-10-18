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

(** Entry points for 'setup.ml' in dev mode
    @author Sylvain Le Gall
  *)

open OASISTypes

type t = 
    { 
      oasis_cmd : prog; (** Program to regenerate the build system. *)
    }

(** Run 'oasis setup' command line tool to regenerate a build system and 
    and run it.
  *)
val update_and_run : t -> unit

(** Create a 'setup.ml' that calls {!update_and_run}. {b Not exported}.
  *)
val make :
  ?oasis_exec:string ->
  OASISPlugin.context_act -> 
  package -> 
  OASISPlugin.context_act * t

(** Dump [ODN.t]. {b Not exported}.
  *)
val odn_of_t : t -> ODN.t
