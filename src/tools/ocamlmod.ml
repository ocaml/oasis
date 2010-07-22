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

(** Tool to transform a source module into an OCaml module
    @author Sylvain Le Gall
  *)

let dump_ml chn_out fn =
  (* filename that should be used to point to source file *)
  let real_fn =
    let pwd = 
      FileUtil.pwd () 
    in
    let pwd =
      (* Translate file from build directory to source
       * directory
       *)
      if FilePath.basename pwd = "_build" then
        FilePath.dirname pwd
      else
        pwd
    in
      FileUtil.readlink (FilePath.make_absolute pwd fn)
  in

  let () = 
    (* Warn if not a .ml file *)
    if not (Filename.check_suffix fn ".ml") then
      prerr_endline ("'"^fn^"' doesn't end with .ml")
  in

  let modname =
    String.capitalize 
      (Filename.basename
         (Filename.chop_suffix fn ".ml"))
  in

  let export_extract chn_out fn = 
    let is_rgxp s_rgxp = 
      let rgxp = 
        Pcre.regexp s_rgxp 
      in
        fun s -> Pcre.pmatch ~rex:rgxp s
    in
    let with_odn = 
      Pcre.regexp "with +odn($| )"
    in
    let type_conv_path =
      Pcre.regexp "TYPE_CONV_PATH +\"[^\"]*\""
    in
    let is_export_end =
      is_rgxp "\\(\\* +END +EXPORT +\\*\\)"
    in
    let is_export_start = 
      is_rgxp "\\(\\* +START +EXPORT +\\*\\)"
    in
    let is_beg_comment = 
      is_rgxp "\\(\\*.*" 
    in

    let line_num = 
      ref 0
    in
    let chn_in =
      open_in fn 
    in
    let input_line () =
      incr line_num;
      input_line chn_in
    in

    let rec skip_header line = 
      if is_export_start line then
        start_export (input_line ())
      else if is_beg_comment line then
        skip_header (input_line ())
      else
        start_export line

    and skip_export line = 
      if is_export_start line then
        start_export (input_line ())
      else
        skip_export (input_line ())

    and start_export line =
      Printf.fprintf chn_out "# %d %S\n" !line_num real_fn;
      parse_body line

    and parse_body line =
        if is_export_end line then
          skip_export (input_line ())
        else
          begin
            let line = 
              (* Remove ODN elements *)
              List.fold_left
                (fun str rgxp -> Pcre.replace ~rex:rgxp str)
                line
                [with_odn; type_conv_path]
            in
              Printf.fprintf chn_out "  %s\n" line;
              parse_body (input_line ())
          end
    in
      begin
        try
          skip_header (input_line ());
        with End_of_file ->
          ()
      end;
      close_in chn_in
  in

    Printf.fprintf chn_out "module %s = struct\n" modname;
    export_extract chn_out fn;
    Printf.fprintf chn_out "end\n\n"

let process chn_in curdir chn_out =
  try
    while true do 
      let fn =
        input_line chn_in
      in
      let real_fn =
        if Filename.is_relative fn then
          Filename.concat curdir fn
        else
          fn
      in
        if not (Sys.file_exists real_fn) then
          failwith ("Cannot find file '"^real_fn^"'");
        dump_ml chn_out real_fn
    done
  with End_of_file ->
    ()

let process_modfile fn =
  let chn_out =
    open_out ((Filename.chop_extension fn)^".ml")
  in
  let chn_in =
    open_in fn
  in
    process chn_in (Filename.dirname fn) chn_out;
    close_in chn_in;
    close_out chn_out

let () = 
  let lst =
    ref []
  in
    Arg.parse
      []
      (fun fn -> lst := fn :: !lst)
      "OCaml module generator written by Sylvain Le Gall";

    List.iter
      process_modfile 
      (List.rev !lst)
