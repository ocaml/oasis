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

(** Test OASISValues defined fields
    @author Sylvain Le Gall
  *)

open Printf
open TestCommon
open OASISValues
open OUnit

let tests_url =
  List.map
    (fun txt ->
     TestCase (fun () ->
               let _a = url.parse ~ctxt:!oasis_ctxt txt in
               ()))
    ["https://oasis.forge.ocamlcore.org";
     "svn://scm.ocamlcore.org/svnroot/cryptokit/tags";
     "svn+ssh://scm.ocamlcore.org/svnroot/cryptokit/tags";
     "http://foo.org/~bar/baz";
     "git+ssh://test.com";
    ]

let tests_command_line_options =
  List.map
    (fun (s, r) ->
     let title = sprintf "command_line_options: %S" s in
     title >:: (fun () ->
                let p = command_line_options.parse ~ctxt:!oasis_ctxt s in
                assert_equal p r))
  ["", [];
   "a", ["a"];
   "a b", ["a"; "b"];
   "a b c", ["a"; "b"; "c"];
   "a'b'", ["ab"];        "a 'b'",   ["a"; "b"];
   "a\"b\"", ["ab"];      "a \"b\"", ["a"; "b"];
   "a'b c'", ["ab c"];    "a 'b c'",   ["a"; "b c"];
   "a\"b c\"", ["ab c"];  "a \"b c\"", ["a"; "b c"];
   "a 'b\"'", ["a"; "b\""];
   "a \"b\\\"\"", ["a"; "b\""];
   "a\"b\\\"\"c", ["ab\"c"];
   "a \"b \\\"\" c", ["a"; "b \""; "c"];
   "a\\ b c", ["a b"; "c"];
   "a\\b \"a\\b\"", ["ab"; "a\\b"]; (* \ in quoted strings *)
   "a\\ b \"a\\b\\\"\"", ["a b"; "a\\b\""];
   "a\\ b \"a\\b\\$\"", ["a b"; "a\\b$"];
   "a\\$ b \"a\\b\\\\\"", ["a$"; "b"; "a\\b\\"];
   (* Real world problem *)
   "-DEXTERNAL_EXP10 -L/sw/lib \"-framework vecLib\"",
   ["-DEXTERNAL_EXP10"; "-L/sw/lib"; "-framework vecLib"];
  ]


let tests =
  "Values" >:::
  (tests_url @ tests_command_line_options)
