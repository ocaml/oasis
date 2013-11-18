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


(** OCamlbuild extension, copied from
  * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
  * by N. Pouillard and others
  *
  * Updated on 2009/02/28
  *
  * Modified by Sylvain Le Gall
  *)
open Ocamlbuild_plugin


(* these functions are not really officially exported *)
let run_and_read =
  Ocamlbuild_pack.My_unix.run_and_read


let blank_sep_strings =
  Ocamlbuild_pack.Lexers.blank_sep_strings


let split s ch =
  let buf = Buffer.create 13 in
  let x = ref [] in
  let flush () =
    x := (Buffer.contents buf) :: !x;
    Buffer.clear buf
  in
    String.iter
      (fun c ->
         if c = ch then
           flush ()
         else
           Buffer.add_char buf c)
      s;
    flush ();
    List.rev !x


let split_nl s = split s '\n'


let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s


(* This lists all supported packages. *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")


(* Mock to list available syntaxes. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]


(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]


let dispatch =
  function
    | Before_options ->
        (* By using Before_options one let command line options have an higher
         * priority on the contrary using After_options will guarantee to have
         * the higher priority override default commands by ocamlfind ones *)
        Options.ocamlc     := ocamlfind & A"ocamlc";
        Options.ocamlopt   := ocamlfind & A"ocamlopt";
        Options.ocamldep   := ocamlfind & A"ocamldep";
        Options.ocamldoc   := ocamlfind & A"ocamldoc";
        Options.ocamlmktop := ocamlfind & A"ocamlmktop"

    | After_rules ->

        (* When one link an OCaml library/binary/package, one should use
         * -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option when
         * compiling, computing dependencies, generating documentation and
         * linking. *)
        List.iter
          begin fun pkg ->
            let base_args = [A"-package"; A pkg] in
            let syn_args = [A"-syntax"; A "camlp4o"] in
            let args =
        (* Heuristic to identify syntax extensions: whether they end in
         * ".syntax"; some might not *)
              if Filename.check_suffix pkg "syntax"
              then syn_args @ base_args
              else base_args
            in
            flag ["ocaml"; "compile";  "pkg_"^pkg] & S args;
            flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S args;
            flag ["ocaml"; "doc";      "pkg_"^pkg] & S args;
            flag ["ocaml"; "link";     "pkg_"^pkg] & S base_args;
            flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S args;
          end
          (find_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is useless
         * when linking. *)
        List.iter begin fun syntax ->
        flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "infer_interface"; "syntax_"^syntax] &
              S[A"-syntax"; A syntax];
        end (find_syntaxes ());

        (* The default "thread" tag is not compatible with ocamlfind.
         * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
         * options when using this tag. When using the "-linkpkg" option with
         * ocamlfind, this module will then be added twice on the command line.
         *
         * To solve this, one approach is to add the "-thread" option when using
         * the "threads" package using the previous plugin.
         *)
        flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
        flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
        flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
        flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

    | _ ->
        ()
