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

(* Load topfind with checks. *)
let () =
  begin
    try
      Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
    with Not_found ->
      ()
  end;
  (* Create a reference error to compare it with a real load. *)
  let ref_error =
    let str =
      (* It should be impossible to have a file with \000 in it, hence it should
       * fail.
      *)
      Topdirs.dir_use Format.str_formatter "topfin\000";
      Format.flush_str_formatter ()
    in
    for i = 0 to (String.length str) - 1 do
      if str.[i] = '\000' then
        str.[i] <- 'd'
    done;
    str
  in
  let status =
    Topdirs.dir_use Format.str_formatter "topfind";
    Format.flush_str_formatter ()
  in
  if status = ref_error then
    failwith
      "Unable to load 'topfind'. Please install findlib and check that \
       topfind.cmo is present."
;;

(* Check presence of oasis.dynrun. *)
let () =
  try
    let _str : string = Findlib.package_directory "oasis.dynrun" in
    ()
  with  Fl_package_base.No_such_package (_, _) ->
    failwith
      "Cannot load findlib package 'oasis.dynrun'. Please install \
       oasis (>= 0.3.0) and check that 'oasis.dynrun' is present \
       in the list of findlib packages (`ocamlfind list`)."
;;
#require "oasis.dynrun";;
open OASISDynRun;;
