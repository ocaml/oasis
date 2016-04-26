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


(** OCamlbuild extension, copied from
  * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
  * by N. Pouillard and others
  *
  * Updated on 2009/02/28
  *
  * Modified by Sylvain Le Gall
*)
open Ocamlbuild_plugin

type conf =
  { no_automatic_syntax: bool;
  }

(* these functions are not really officially exported *)
let run_and_read =
  Ocamlbuild_pack.My_unix.run_and_read


let blank_sep_strings =
  Ocamlbuild_pack.Lexers.blank_sep_strings


let exec_from_conf exec =
  let exec =
    let env_filename =
      Pathname.basename (Lazy.force BaseEnvLight.default_filename) in
    let env = BaseEnvLight.load ~filename:env_filename ~allow_empty:true () in
    try
      BaseEnvLight.var_get exec env
    with Not_found ->
      Printf.eprintf "W: Cannot get variable %s\n" exec;
      exec
  in
  let fix_win32 str =
    if Sys.os_type = "Win32" then begin
      let buff = Buffer.create (String.length str) in
      (* Adapt for windowsi, ocamlbuild + win32 has a hard time to handle '\\'.
      *)
      String.iter
        (fun c -> Buffer.add_char buff (if c = '\\' then '/' else c))
        str;
      Buffer.contents buff
    end else begin
      str
    end
  in
  fix_win32 exec

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

(* ocamlfind command *)
let ocamlfind x = S[Sh (exec_from_conf "ocamlfind"); x]

(* This lists all supported packages. *)
let find_packages () =
  List.map before_space (split_nl & run_and_read (exec_from_conf "ocamlfind" ^ " list"))


(* Mock to list available syntaxes. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]


let well_known_syntax = [
  "camlp4.quotations.o";
  "camlp4.quotations.r";
  "camlp4.exceptiontracer";
  "camlp4.extend";
  "camlp4.foldgenerator";
  "camlp4.listcomprehension";
  "camlp4.locationstripper";
  "camlp4.macro";
  "camlp4.mapgenerator";
  "camlp4.metagenerator";
  "camlp4.profiler";
  "camlp4.tracer"
]


let dispatch conf =
  function
    | After_options ->
      (* By using Before_options one let command line options have an higher
       * priority on the contrary using After_options will guarantee to have
       * the higher priority override default commands by ocamlfind ones *)
      Options.ocamlc     := ocamlfind & A"ocamlc";
      Options.ocamlopt   := ocamlfind & A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop";
      Options.ocamlmklib := ocamlfind & A"ocamlmklib"

    | After_rules ->

      (* Avoid warnings for unused tag *)
      flag ["tests"] N;

      (* When one link an OCaml library/binary/package, one should use
       * -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      (* For each ocamlfind package one inject the -package option when
       * compiling, computing dependencies, generating documentation and
       * linking. *)
      List.iter
        begin fun pkg ->
          let base_args = [A"-package"; A pkg] in
          (* TODO: consider how to really choose camlp4o or camlp4r. *)
          let syn_args = [A"-syntax"; A "camlp4o"] in
          let (args, pargs) =
            (* Heuristic to identify syntax extensions: whether they end in
               ".syntax"; some might not.
            *)
            if not (conf.no_automatic_syntax) &&
               (Filename.check_suffix pkg "syntax" ||
                List.mem pkg well_known_syntax) then
              (syn_args @ base_args, syn_args)
            else
              (base_args, [])
          in
          flag ["ocaml"; "compile";  "pkg_"^pkg] & S args;
          flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S args;
          flag ["ocaml"; "doc";      "pkg_"^pkg] & S args;
          flag ["ocaml"; "link";     "pkg_"^pkg] & S base_args;
          flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S args;

          (* TODO: Check if this is allowed for OCaml < 3.12.1 *)
          flag ["ocaml"; "compile";  "package("^pkg^")"] & S pargs;
          flag ["ocaml"; "ocamldep"; "package("^pkg^")"] & S pargs;
          flag ["ocaml"; "doc";      "package("^pkg^")"] & S pargs;
          flag ["ocaml"; "infer_interface"; "package("^pkg^")"] & S pargs;
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
      flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);
      flag ["c"; "pkg_threads"; "compile"] (S[A "-thread"]);
      flag ["ocaml"; "package(threads)"; "compile"] (S[A "-thread"]);
      flag ["ocaml"; "package(threads)"; "doc"] (S[A "-I"; A "+threads"]);
      flag ["ocaml"; "package(threads)"; "link"] (S[A "-thread"]);
      flag ["ocaml"; "package(threads)"; "infer_interface"] (S[A "-thread"]);
      flag ["c"; "package(threads)"; "compile"] (S[A "-thread"]);

    | _ ->
      ()
