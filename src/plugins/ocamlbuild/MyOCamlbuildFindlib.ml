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


let exec_from_conf exec =
  let exec =
    let env_filename = Pathname.basename BaseEnvLight.default_filename in
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

(* ocamlfind command *)
let ocamlfind x = S[Sh (exec_from_conf "ocamlfind"); x]


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
        Options.ocamlmklib := ocamlfind & A"ocamlmklib"

    | After_rules ->

        if not (conf.no_automatic_syntax) then begin
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          (* TODO: consider how to really choose camlp4o or camlp4r. *)
          let syn_args = [A"-syntax"; A "camlp4o"] in
          let get_syn_args pkg =
            (* Heuristic to identify syntax extensions: whether they end in
               ".syntax"; some might not.
            *)
            if Filename.check_suffix pkg "syntax" ||
               List.mem pkg well_known_syntax then
              S syn_args
            else
              N
          in
          pflag ["ocaml"; "compile"] "package" get_syn_args;
          pflag ["ocaml"; "ocamldep"] "package" get_syn_args;
          pflag ["ocaml"; "doc"] "package" get_syn_args;
          pflag ["ocaml"; "infer_interface"] "package" get_syn_args;
        end;

        (* The default "thread" tag is not compatible with ocamlfind.
         * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
         * options when using this tag. When using the "-linkpkg" option with
         * ocamlfind, this module will then be added twice on the command line.
         *
         * To solve this, one approach is to add the "-thread" option when using
         * the "threads" package using the previous plugin.
        *)
        flag ["ocaml"; "package(threads)"; "compile"] (S[A "-thread"]);
        flag ["ocaml"; "package(threads)"; "doc"] (S[A "-I"; A "+threads"]);
        flag ["ocaml"; "package(threads)"; "link"] (S[A "-thread"]);
        flag ["ocaml"; "package(threads)"; "infer_interface"] (S[A "-thread"]);

    | _ ->
        ()
