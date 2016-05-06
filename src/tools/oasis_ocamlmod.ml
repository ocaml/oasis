#!/usr/bin/env ocaml

#load "str.cma";;

(******************************************************************************)
(* ocamlmod: Generate OCaml modules from source files                         *)
(*                                                                            *)
(* Copyright (C) 2011, OCamlCore SARL                                         *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
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

(** Tool to transform a source module into an OCaml module
    @author Sylvain Le Gall
  *)

let dump_ml chn_out fn =
  (* filename that should be used to point to source file *)
  let real_fn =
    let pwd =
      Sys.getcwd ()
    in
    let pwd =
      (* Translate file from build directory to source
       * directory
       *)
      if Filename.basename pwd = "_build" then
        Filename.dirname pwd
      else
        pwd
    in
      Filename.concat pwd fn
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
        Str.regexp s_rgxp
      in
        fun s ->
          Str.string_match rgxp s 0
    in
    let with_odn =
      Str.regexp " +with +odn\\($\\| \\)"
    in
    let type_conv_path =
      Str.regexp "TYPE_CONV_PATH +\"[^\"]*\""
    in
    let is_export_end =
      is_rgxp "(\\* +END +EXPORT +\\*)"
    in
    let is_export_start =
      is_rgxp "(\\* +START +EXPORT +\\*)"
    in
    let is_beg_comment =
      is_rgxp "(\\*.*"
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
      Printf.fprintf chn_out "# %d %S\n" !line_num fn;
      parse_body line

    and parse_body line =
        if is_export_end line then
          skip_export (input_line ())
        else
          begin
            let line =
              (* Remove ODN elements *)
              List.fold_left
                (fun str rgxp -> Str.global_replace rgxp "" str)
                line
                [with_odn; type_conv_path]
            in
              if line = "" then
                Printf.fprintf chn_out "\n"
              else
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

let process_modfile ?out_fn fn =
  let out_fn =
    match out_fn with
      | None -> (Filename.chop_extension fn)^".ml"
      | Some fn -> fn
  in
  let chn_out = open_out out_fn in
  let chn_in = open_in fn in
    process chn_in (Filename.dirname fn) chn_out;
    close_in chn_in;
    close_out chn_out

let () =
  let lst =
    ref []
  in
  let output =
    ref None
  in
    Arg.parse
      ["-o",
       Arg.String (fun str -> output := Some str),
       "fn Output file.";
      ]
      (fun fn -> lst := fn :: !lst)
      "OCaml module generator written by Sylvain Le Gall";

    match !output, !lst with
      | _, [] ->
          failwith "Need at least one .mod file."
      | Some out_fn, [in_fn] ->
          process_modfile ~out_fn in_fn
      | Some _, lst ->
          failwith "Cannot use -o with multiple .mod files."
      | None, lst ->
          List.iter process_modfile (List.rev lst)
