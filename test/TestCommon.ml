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


(** Common utilities for testing
    @author Sylvain Le Gall
  *)


IFDEF HAS_GETTEXT THEN
module Gettext =
  Gettext.Program
    (struct
       let textdomain   = "oasis"
       let codeset      = None
       let dir          = None
       let dependencies = Gettext.init @ OASISGettext.init
     end)
    (GettextStub.Native)
ELSE
module Gettext =
struct
  let init = [], ""
end
ENDIF


open OUnit2


module MapString = Map.Make(String)
module SetString = Set.Make(String)


let is_native =
  Conf.make_bool
    "is_native"
    (bool_of_string (BaseStandardVar.is_native ()))
    "Wether native compilation possible."


let native_dynlink =
  Conf.make_bool
    "native_dynlink"
    (bool_of_string (BaseStandardVar.native_dynlink ()))
    "Wether native dynlink is possible."


let oasis_exec = Conf.make_exec "oasis"
let ocamlmod_exec = Conf.make_exec "ocamlmod"
let fake_ocamlfind_exec = Conf.make_exec "fake_ocamlfind"
(* TODO: add make_string_list to OUnit2. *)
let oasis_args ctxt = []
let oasis_ctxt = OASISContext.quiet
let oasis_ignore_plugin_ctxt =
  {oasis_ctxt with OASISContext.ignore_plugins = true}


let long =
  Conf.make_bool
    "long"
    true
    "Don't run long tests."


let skip_long_test ctxt =
  skip_if (not (long ctxt)) "Long test."


let example_dir =
  let value =
    Conf.make_string
      "example_dir"
      "../examples/"
      "Examples directory."
  in
    fun ctxt ->
      let fn = value ctxt in
        if FilePath.is_relative fn then
          FilePath.make_absolute (FileUtil.pwd ()) fn
        else
          fn


let in_example_dir test_ctxt lst =
  FilePath.make_filename ((example_dir test_ctxt) :: lst)


module Output =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end


module DiffSetOutput = OUnitDiff.SetMake (Output)
module DiffListOutput = OUnitDiff.ListSimpleMake (Output)


(* Assert checking that command run well *)
let assert_command ~ctxt
      ?chdir
      ?exit_code
      ?output
      ?extra_env
      ?(unorder=false)
      (* TODO: this should be true, but too many errors: fix this. *)
      ?(check_output=false)
      cmd args =
  let foutput =
    let read_check_output strm =
      let output =
        let buff =
          Buffer.create 13
        in
          Stream.iter (Buffer.add_char buff) strm;
          Buffer.contents buff
      in
      let lines = OASISString.nsplit output '\n' in
      (* Check for warnings/errors. *)
      if check_output then
        List.iter
          (fun line ->
             non_fatal ctxt
               (fun test_ctxt ->
                  List.iter
                    (fun (what, fmt) ->
                       if OASISString.starts_with ~what line then
                         assert_failure (Printf.sprintf fmt line))
                    ["E:", ""^^"Error in line %S";
                     "W:", ""^^"Warning in line %S";
                     "Warning", ""^^"Warning in line %S"]))
          lines;
      lines
    in
    match output with
      | Some exp_output ->
          let foutput strm =
            let rel_output = read_check_output strm in
            let exp_output = OASISString.nsplit exp_output '\n' in

            let assert_equal_diff ~msg t1 t2 =
              if unorder then
                DiffSetOutput.assert_equal
                  ~msg
                  (DiffSetOutput.of_list t1)
                  (DiffSetOutput.of_list t2)
              else
                DiffListOutput.assert_equal
                  ~msg
                  (DiffListOutput.of_list t1)
                  (DiffListOutput.of_list t2)
            in
              assert_equal_diff
                ~msg:(Printf.sprintf "'%s' command output"
                        (String.concat " " (cmd :: args)))
                exp_output
                rel_output
          in
            Some foutput

      | None ->
          Some
            (fun strm ->
               let _lst: string list = read_check_output strm in
                 ())
  in
  let env =
    let readd lst nm =
      try
        (nm^"="^(Sys.getenv nm)) :: lst
      with Not_found ->
        lst
    in
    let min_env =
      if Sys.os_type = "Win32" then
        Array.to_list (Unix.environment ())
      else
        List.fold_left readd [] ["PATH"; "OCAMLPATH"]
    in
    let extra_env =
      match extra_env with
        | Some lst ->
            List.map (fun (k, v) -> k^"="^v) lst

        | None ->
            []
    in
      Some (Array.of_list (extra_env @ min_env))
  in
    assert_command
      ~ctxt ?chdir ?foutput ?env ?exit_code ~use_stderr:true
      cmd args


let assert_oasis_cli ~ctxt ?chdir ?exit_code ?output ?extra_env ?unorder args  =
  (* TODO: transfert chdir to -C chdir. *)
  assert_command ~ctxt ?chdir ?exit_code ?output ?extra_env ?unorder
    (oasis_exec ctxt) ((oasis_args ctxt) @ args)


let file_content fn =
  let chn = open_in_bin fn in
  let size = in_channel_length chn in
  let buff = Buffer.create size in
    Buffer.add_channel buff chn size;
    close_in chn;
    Buffer.contents buff


let dbug_file_content test_ctxt fn =
  (* TODO: re-enable when OUnit will discard it for JUnit.xml.
  logf test_ctxt `Info "Content of %S:" fn;
  logf test_ctxt `Info "%s" (file_content fn)
   *)
  ()

(* Start a timer for [str]. *)
let timer_start str =
  Unix.gettimeofday (), str

(* Stop a timer and output its data in the log. *)
let timer_stop test_ctxt (time_start, str) =
  logf test_ctxt `Info "Time spent in '%s': %fs"
    str ((Unix.gettimeofday ()) -. time_start)


let skip_test_on_non_native_arch lst =
  let skip_non_native =
    OUnitTest.test_decorate
      (fun f ->
         fun test_ctxt ->
           skip_if
             (* Use the real is_native function and skip if on non native
              * arch.
              *)
             (not (is_native test_ctxt))
             "only run on native arch";
           f test_ctxt)
  in
  List.map skip_non_native lst
