(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

let win_quote_needed str =
  let len = String.length str in
  if len = 0 then
    true
  else
    let rec iter str i =
      if i < 0 then false
      else match str.[i] with
      | '/'| '"'| ' '| '\t' | '\n' | '\x0B' -> true
      | _ -> iter str (pred i)
    in
    iter str (pred len)


let run_bash ~ctxt ?f_exit_code ?(quote=true) cmd args =
  let fn = Filename.temp_file "oasis-" ".sh" in
  let fn_deleted = ref false in
  try
    let ch = open_out_bin fn in
    let ch_closed = ref false in
    (try
      let cmd = match quote with
      | false -> cmd
      | true  -> OASISHostPath.quote (OASISHostPath.of_unix cmd)
      in
      output_string ch cmd;
      List.iter ( fun s -> output_char ch ' '; output_string ch s ) args ;
      output_char ch '\n';
      ch_closed:=true ;
      close_out ch;
      let bash = !OASISHostPath.bash_cmd () in
      let add_quotes = ref false in
      let shell_cmd = match Sys.os_type = "Win32" with
      | false -> Filename.quote bash
      | true -> match win_quote_needed bash with
        | true -> add_quotes := true; Filename.quote bash
        | false -> bash
      in
      let cmdline_orig = String.concat " " (cmd :: args)
      and cmdline =
        let s = shell_cmd ^ " " ^ (Filename.quote fn) in
        match !add_quotes with
        | true -> "\"" ^ s ^ "\""
        | false -> s
      in
      info ~ctxt (f_ "Running command '%s'") cmdline_orig;
      let ret = Sys.command cmdline in
      fn_deleted := true;
      Sys.remove fn;
      match f_exit_code, ret with
      | None, 0 -> ()
      | None, i ->
        failwithf
          (f_ "Command '%s' terminated with error code %d")
          cmdline_orig i
      | Some f, i ->
          f i
     with
     | x when !ch_closed = false ->
       close_out_noerr ch; raise x )
  with
  | x when !fn_deleted = false ->
    (try Sys.remove fn with _ -> () ) ;
    raise x


(* TODO: I don't like this quote, it is there because $(rm) foo expands to
 * 'rm -f' foo...
 *)

let run_default ~ctxt ?f_exit_code ?(quote=true) cmd args =
  let add_quotes = ref false in
  let cmd =
    if quote then
      if Sys.os_type = "Win32" then
        if win_quote_needed cmd = false then cmd
        else (
          (* Double the 1st double quote... win32... sigh *)
          (* above comment seems false. The whole string must be quoted *)
          add_quotes := true;
          Filename.quote cmd
        )
      else
        Filename.quote cmd
    else
      cmd
  in
  let cmdline =
    let s = String.concat " " (cmd :: args) in
    match !add_quotes with
    | true -> "\"" ^ s ^ "\""
    | false -> s
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

let run ~ctxt ?f_exit_code ?quote cmd args =
  match OASISHostPath.use_bash () with
  | false -> run_default ~ctxt ?f_exit_code ?quote cmd args
  | true  -> run_bash ~ctxt ?f_exit_code ?quote cmd args

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
