(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


(** Test OASISValues defined fields
    @author Sylvain Le Gall
  *)


open Printf
open TestCommon
open OASISValues
open OUnit2


let tests_url =
  List.map
    (fun txt ->
       test_case 
         (fun test_ctxt ->
            let _a = url.parse ~ctxt:(oasis_ctxt test_ctxt) txt in ()))
    ["https://oasis.forge.ocamlcore.org";
     "svn://scm.ocamlcore.org/svnroot/cryptokit/tags";
     "svn+ssh://scm.ocamlcore.org/svnroot/cryptokit/tags";
     "http://foo.org/~bar/baz";
     "git+ssh://test.com";
    ]


let tests_command_line_options =
  List.map
    (fun (s, r) ->
     (sprintf "command_line_options: %S" s) >::
      (fun test_ctxt ->
         let p = command_line_options.parse ~ctxt:(oasis_ctxt test_ctxt) s in
           assert_equal p r))
  ["", [];
   "a", ["a"];
   "a b", ["a"; "b"];
   "a b c", ["a"; "b"; "c"];
   " a  b   c  ", ["a"; "b"; "c"];
   "a'b'", ["ab"];        "a 'b'",   ["a"; "b"];
   "a'b'c", ["abc"];      "a 'b' c", ["a"; "b"; "c"];
   "a\"b\"", ["ab"];      "a \"b\"", ["a"; "b"];
   "a'b c'", ["ab c"];    "a 'b c'",   ["a"; "b c"];
   "a\"b c\"", ["ab c"];  "a \"b c\"", ["a"; "b c"];
   "a 'b\"'", ["a"; "b\""];
   "a \"b\\\"\"", ["a"; "b\""];
   "a\"b\\\"\"c", ["ab\"c"];
   "a \"b \\\"\" c", ["a"; "b \""; "c"];
   "a\\ b c", ["a b"; "c"];
   "\\", []; (* corner case, '\\' consisered as continuation char *)
   "a\\b \"a\\b\"", ["ab"; "a\\b"]; (* \ in quoted strings *)
   "a\\ b \"a\\b\\\"\"", ["a b"; "a\\b\""];
   "a\\ b \"a\\b\\$\"", ["a b"; "a\\b$"];
   "a\\$ b \"a\\b\\\\\"", ["a$"; "b"; "a\\b\\"];
   (* Substitutions *)
   "$a11", ["${a11}"];
   "${a1}1", ["${a1}1"];
   "$(a}b)", ["$(a}b)"];
   "${a)b}", ["${a)b}"];
   "$(a '}' b)", ["$(a '}' b)"];
   (* Real world problem *)
   "-DEXTERNAL_EXP10 -L/sw/lib \"-framework vecLib\"",
   ["-DEXTERNAL_EXP10"; "-L/sw/lib"; "-framework vecLib"];
  ]


let tests_posix_quoting =
  List.map
    (fun s ->
      (sprintf "OASISUtils.POSIXShell: %S" s) >::
      (fun _ ->
         let q = OASISUtils.POSIXShell.escape s in
         let u =
           if q.[0] = '"' then
             let q' = String.sub q 1 (String.length q - 2) in
               OASISUtils.POSIXShell.unescape q'
           else q
         in
           assert_equal s u))
    [""; "a b"; "a'b"; "echo \'abcd"; "a\\b"]


let tests =
  "Values" >:::
  (tests_url @ tests_command_line_options @ tests_posix_quoting)
