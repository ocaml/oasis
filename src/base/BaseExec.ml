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

(** Running commands 
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISUtils

(** Run a command 
  *)
let run cmd args =
  let cmdline =
    String.concat " " (cmd :: args)
  in
    OASISMessage.info (f_ "Running command '%s'") cmdline;
    match Sys.command cmdline with 
      | 0 ->
          ()
      | i ->
          failwithf2
            (f_ "Command '%s' terminated with error code %d")
            cmdline i

(** Run a command and returns its output
  *)
let run_read_output cmd args =
  let fn = 
    Filename.temp_file "oasis-" ".txt"
  in
  let () = 
    try
      run cmd (args @ [">"; fn])
    with e ->
      Sys.remove fn;
      raise e
  in
  let chn =
    open_in fn
  in
  let routput =
    ref []
  in
    (
      try
        while true do 
          routput := (input_line chn) :: !routput
        done
      with End_of_file ->
        ()
    );
    close_in chn;
    Sys.remove fn;
    List.rev !routput

(** Run a command and returns only first line 
  *)
let run_read_one_line cmd args = 
  match run_read_output cmd args with 
    | [fst] -> 
        fst
    | lst -> 
        failwithf1
          (f_ "Command return unexpected output %S")
          (String.concat "\n" lst)

(* END EXPORT *)
