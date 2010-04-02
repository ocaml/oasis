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

(** Message to user
    @author Sylvain Le Gall
  *)

open OASISGettext

let verbose =
  ref true

let debug =
  ref false

(** Command line arguments
  *)
let args () =
  ["-quiet",
   Arg.Clear verbose,
   (s_ " Run quietly");

   "-debug",
   Arg.Set debug,
   (s_ " Output debug message")]

(**/**)
let generic_message ?(after=ignore) cond beg fmt =
  if cond then
    begin
      Printf.fprintf stderr "%s: " beg;
      Printf.kfprintf 
        (fun chn -> 
           Printf.fprintf chn "\n%!";
           after ())
        stderr
        fmt
    end
  else
    begin
      Printf.ifprintf 
        stderr
        fmt
    end
(**/**)


(** Print a debug message
  *)
let debug fmt =
  generic_message !debug "D" fmt

(** Print information message.
  *)
let info fmt = 
  generic_message !verbose "I" fmt

(** Print a warning message 
  *)
let warning fmt =
  generic_message !verbose "W" fmt

(** Print an error message and exit.
  *)
let error fmt =
  generic_message ~after:(fun () -> exit 1) !verbose "E" fmt
