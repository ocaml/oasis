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


open BaseEnv
open BaseMessage
open OASISTypes
open OASISExpr
open OASISGettext


let test lst pkg extra_args =

  let one_test (failure, n) (test_plugin, cs, test) =
    if var_choose
        ~name:(Printf.sprintf
            (f_ "test %s run")
            cs.cs_name)
        ~printer:string_of_bool
        test.test_run then
      begin
        let () =
          info (f_ "Running test '%s'") cs.cs_name
        in
        let back_cwd =
          match test.test_working_directory with
            | Some dir ->
              let cwd =
                Sys.getcwd ()
              in
              let chdir d =
                info (f_ "Changing directory to '%s'") d;
                Sys.chdir d
              in
              chdir dir;
              fun () -> chdir cwd

            | None ->
              fun () -> ()
        in
        try
          let failure_percent =
            BaseCustom.hook
              test.test_custom
              (test_plugin pkg (cs, test))
              extra_args
          in
          back_cwd ();
          (failure_percent +. failure, n + 1)
        with e ->
          begin
            back_cwd ();
            raise e
          end
      end
    else
      begin
        info (f_ "Skipping test '%s'") cs.cs_name;
        (failure, n)
      end
  in
  let failed, n =
    List.fold_left
      one_test
      (0.0, 0)
      lst
  in
  let failure_percent =
    if n = 0 then
      0.0
    else
      failed /. (float_of_int n)
  in
  let msg =
    Printf.sprintf
      (f_ "Tests had a %.2f%% failure rate")
      (100. *. failure_percent)
  in
  if failure_percent > 0.0 then
    failwith msg
  else
    info "%s" msg;

  (* Possible explanation why the tests where not run. *)
  if OASISFeatures.package_test OASISFeatures.flag_tests pkg &&
     not (bool_of_string (BaseStandardVar.tests ())) &&
     lst <> [] then
    BaseMessage.warning
      "Tests are turned off, consider enabling with \
       'ocaml setup.ml -configure --enable-tests'"
