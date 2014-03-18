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




(* In general, there is no chance to quote properly with the current
 * settings. ( This is only a problem, if BaseCustom.run is used (e.g
 * test commands, PreConfigure,....). Most of the time, OASISExec.run
 * is used with enabled quoting.)
 *
 * "$rm" should ideally expand to 'rm -f' (no quotes), but "$test_exec"
 * should ideally expand to '"C:\Program Files\dir\test.exe"' (with
 * quotes). Paths with spaces are common on Windows, so this problem
 * can't be ignored.
 *
 * Using a command with additional parameters is quite useful, not only
 * for trivial cases like $rm. You could add an additional parameter
 * for $make in order to use a special compatibility mode, on windows
 * you can use it to inform ocaml that a certain program is a shell
 * script ('sh.exe pcre-config').
 *
 * An additional parameter (e.g. $rm_switches) would be ugly,
 * especially for *nix users, who don't use space characters in their
 * installation paths anyway.
 *
 * I use the following workaround, which should work most of the time:
 *
 * - if cmd doesn't contains spaces or other suspicious characters, it
 * can be quoted in the usual way (not ambigous, I think)
 *
 * - if cmd does contain spaces, a file with this name exists, and the
 * beginning of cmd looks like a absolute pathname
 * ('\\test\dir\foo.exe' or "C:\\sa df\\foo.exe" - not "foo.exe" ), I
 * will also quote it. (ambigous, there could be "C:\bin\rm.exe" and
 * "C:\bin\rm -f.exe").  Relative filenames are not considered, because
 * I assume the source code folder contains only well named files and
 * relative paths like "../../make.exe" are uncommon (autoconf even
 * rejects them) *)


(* stricter settings as for regular windows batch lines
 * necessary because of shell comannds like:
 *   LC_ALL=C make ....
 *)
let is_dubious_char = function
  | '~' | ':' | '.' | '-' | '_' | '/' | '\\' -> false
  | c ->
    OASISString.is_digit c = false &&
    OASISString.is_alpha c = false


let win_quote_needed str =
  let f = function
    (* this list is not exhaustive. Feel free to added common chars, that
     * can be passed to cmd.exe without quoting *)
    | 'a' .. 'z'  | 'A' .. 'Z' | '0' .. '9'
    | '_' | '-' | '~' | '.' | ':' | ',' | '\\' -> false
    | _ -> true
  in
    str = "" || OASISString.exists f str

let is_simple_command str =
  String.length str > 0 &&
  not (OASISString.exists is_dubious_char str)

let is_path_sep = function
  | '/' | '\\' -> true
  | _ -> false

let starts_with_absolute_path cmd =
  let len = String.length cmd in
    if len < 3 then
      false
    else
      let c0 = cmd.[0] in
      let c1 = cmd.[1] in
        if is_path_sep c0 && is_path_sep c1 then (* network devices: "//" *)
          true
        else if len = 3 then
          false
        else  (* C:\.... *)
          OASISString.is_alpha c0 && c1 = ':' && is_path_sep cmd.[2]

let exe_exts = lazy
  begin
    let exts =
      try
        OASISString.nsplit
          (Sys.getenv "PATHEXT")
          ';'
      with
        | Not_found -> []
    in
    let exts' =
      List.filter
        ( fun a -> a <> "" && a.[0] = '.' && a <> ".EXE" )
        (List.map String.uppercase exts) (* windows file system doesn't care *)
    in
      ".EXE"::exts' (* .exe first, most common *)
  end

let exe_file_exists fln =
  Sys.file_exists fln ||
  List.exists
    (fun a -> Sys.file_exists ( fln ^ a ) )
    (Lazy.force exe_exts)


let quote_anyway cmd =
  if Sys.os_type <> "Win32" then (* workaround for windows only *)
    false
  else if is_simple_command cmd then
    true
  else
    OASISString.exists OASISString.is_whitespace cmd &&
      starts_with_absolute_path cmd &&
      exe_file_exists cmd


let run_bash ~ctxt ?f_exit_code ?(quote=true) cmd args =
  let fn = Filename.temp_file "oasis-" ".sh" in
  let fn_deleted = ref false in
    try
      begin
        let ch = open_out_bin fn in
        let ch_closed = ref false in
          try
            begin
              let cmd =
                if quote || quote_anyway cmd then
                  OASISHostPath.quote (OASISHostPath.of_unix cmd)
                else
                  cmd
              in
                output_string ch cmd;
                List.iter
                  ( fun s -> output_char ch ' '; output_string ch s )
                  args ;
                output_char ch '\n';
                ch_closed:=true ;
                close_out ch;
                let bash = !OASISHostPath.bash_cmd () in
                let add_quotes = ref false in
                let shell_cmd =
                  if Sys.os_type <> "Win32" then
                    Filename.quote bash
                  else
                    if win_quote_needed bash = false then
                      bash
                    else
                      begin
                        add_quotes := true;
                        Filename.quote bash
                      end
                in
                let cmdline_orig = String.concat " " (cmd :: args) in
                let cmdline =
                  let s = shell_cmd ^ " " ^ (Filename.quote fn) in
                    if !add_quotes then
                      "\"" ^ s ^ "\""
                    else
                      s
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
            end
          with
            | x when !ch_closed = false ->
                close_out_noerr ch;
                raise x
      end
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
    if quote || quote_anyway cmd then
      if Sys.os_type = "Win32" then
        begin
          if win_quote_needed cmd = false then
            cmd
          else
            begin
              (* Double the 1st double quote... win32... sigh *)
              (* Above comment ist false. The whole string must be quoted.
               * However, an error is only triggered, if args contains also
               * quoted parameters *)
              add_quotes := true;
              Filename.quote cmd
            end
        end
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
  if OASISHostPath.use_bash () then
    run_bash ~ctxt ?f_exit_code ?quote cmd args
  else
    run_default ~ctxt ?f_exit_code ?quote cmd args

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
