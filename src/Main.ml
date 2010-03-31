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

(** Main for OASIS *)

open OASISGettext
open OASISTypes
open OASISUtils
open OASISPlugin
open OASISBuiltinPlugins

type action_t =
  | Generate 
  | Quickstart
  | Documentation

let () =

  let action =
    ref Generate
  in

  let oasis_fn =
    ref "_oasis"
  in

  let qckstrt_lvl =
    ref Beginner
  in

  let rdev =
    ref false
  in

  let rsetup_fn =
    ref "setup.ml"
  in

  let (gettext_args, _) =
    OASISGettext.init
  in

  let args = 
    [
      "-C",
      Arg.String (fun str -> Sys.chdir str),
      (s_ "dir Change directory before running.");

      "-quickstart",
      Arg.Unit (fun () -> action := Quickstart),
      (s_ " Launch an helper to write `_oasis` file.");

      (
        let lvls =
          [
            s_ "beginner", Beginner; 
            s_ "intermediate", Intermediate; 
            s_ "expert", Expert;
          ]
        in
          "-quickstart-level",
          Arg.Symbol
            ((List.map fst lvls),
             (fun s -> qckstrt_lvl := List.assoc s lvls)),
          (s_ " Quickstart level, skip questions according to this level.")
      );

      "-documentation",
      Arg.Unit (fun () -> action := Documentation),
      (s_ " Display extended help");

      "-dev",
      Arg.Set rdev,
      (s_ " Create a developper mode setup.ml. It will be automatically \
            updated at each run.");

      "-setup-fn",
      Arg.Set_string rsetup_fn,
      (s_ "fn Change the default name of setup.ml. This option should be \
              used with caution, it is reserved for internal use.");                   

    ] 
    @ OASISMessage.args
    @ gettext_args
  in

  let copyright = 
    s_ "OASIS (C) 2009-2010 OCamlCore SARL"
  in

  let msg =
    s_ "OASIS [options] -action [action-options]"
  in

  let () = 
    try 
      Arg.parse_argv 
        Sys.argv
        (Arg.align args)
        (failwithf1 (f_ "Don't know what to do with '%s'"))
        (Printf.sprintf "%s\n\n%s\n\n" copyright msg)
    with 
      | Arg.Bad txt ->
          prerr_endline txt;
          exit 1
      | Arg.Help txt ->
          prerr_endline txt;
          exit 0
  in

    try 
      match !action with 
        | Generate ->
            begin
              let pkg =
                OASIS.from_file !oasis_fn 
              in
                BaseGenerate.generate pkg !rdev !rsetup_fn
            end
        | Quickstart ->
            begin
              let fn =
                "_oasis"
              in
              let chn = 
                open_out_gen 
                  [Open_wronly; Open_creat; Open_excl; Open_text] 
                  0o644
                  "_oasis"
              in
              let fmt = 
                Format.formatter_of_out_channel chn
              in
                Printf.printf (f_ "Creating %s file\n%!") fn;
                OASISQuickstart.quickstart 
                  fmt
                  !qckstrt_lvl;
                Format.pp_print_flush fmt ();
                close_out chn
            end
        | Documentation ->
            begin
              OASISHelp.pp_help Format.std_formatter args msg
            end
    with Failure s ->
      begin
        prerr_endline s;
        exit 1
      end
