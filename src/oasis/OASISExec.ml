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


open OASISGettext
open OASISUtils
open OASISMessage


(* TODO: I don't like this quote, it is there because $(rm) foo expands to
 * 'rm -f' foo...
*)
let run ~ctxt ?f_exit_code ?(quote=true) cmd args =
  let cmd =
    if quote then
      if Sys.os_type = "Win32" then
        if String.contains cmd ' ' then
          (* Double the 1st double quote... win32... sigh *)
          "\""^(Filename.quote cmd)
        else
          cmd
      else
        Filename.quote cmd
    else
      cmd
  in
  let cmdline =
    String.concat " " (cmd :: args)
  in
  info ~ctxt (f_ "Running command '%s'") cmdline;
  match f_exit_code, Sys.command cmdline with
    | None, 0 -> ()
    | None, i ->
      failwithf
        (f_ "Command '%s' terminated with error code %d")
        cmdline i
    | Some f, i ->
      f i


let run_read_output ~ctxt ?f_exit_code cmd args =
  let fn =
    Filename.temp_file "oasis-" ".txt"
  in
  try
    begin
      let () =
        run ~ctxt ?f_exit_code cmd (args @ [">"; Filename.quote fn])
      in
      let chn =
        open_in fn
      in
      let routput =
        ref []
      in
      begin
        try
          while true do
            routput := (input_line chn) :: !routput
          done
        with End_of_file ->
          ()
      end;
      close_in chn;
      Sys.remove fn;
      List.rev !routput
    end
  with e ->
    (try Sys.remove fn with _ -> ());
    raise e


let run_read_one_line ~ctxt ?f_exit_code cmd args =
  match run_read_output ~ctxt ?f_exit_code cmd args with
    | [fst] ->
      fst
    | lst ->
      failwithf
        (f_ "Command return unexpected output %S")
        (String.concat "\n" lst)
