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

(** Functions common to OCamlbuild build and doc plugin
  *)

open OASISGettext
open BaseEnv
open BaseStandardVar

let ocamlbuild_clean_ev =
  "ocamlbuild-clean"

let ocamlbuildflags =
  var_define
    ~short_desc:(fun () -> "OCamlbuild additional flags")
    "ocamlbuildflags"
    (lazy "")

(** Fix special arguments depending on environment *)
let fix_args args extra_argv =
  List.flatten
    [
      if (os_type ()) = "Win32" then
        [
          "-classic-display"; 
          "-no-log"; 
          "-no-links";
          "-install-lib-dir"; 
          (Filename.concat (standard_library ()) "ocamlbuild")
        ] 
      else
        [];
  
      if not (bool_of_string (is_native ())) || (os_type ()) = "Win32" then
        [
          "-byte-plugin" 
        ]
      else
        [];
      args;

      if bool_of_string (ocaml_with_debug ()) then
        ["-tag"; "debug"]
      else
        [];

      if bool_of_string (ocaml_with_profile ()) then
        ["-tag"; "profile"]
      else
        [];

      OASISUtils.split ' ' (ocamlbuildflags ());

      Array.to_list extra_argv;
    ]

(** Run 'ocamlbuild -clean' if not already done *)
let run_clean extra_argv =
  let extra_cli =
    String.concat " " (Array.to_list extra_argv)
  in
    (* Run if never called with these args *)
    if not (BaseLog.exists ocamlbuild_clean_ev extra_cli) then
      begin
        BaseExec.run (ocamlbuild ()) (fix_args ["-clean"] extra_argv);
        BaseLog.register ocamlbuild_clean_ev extra_cli;
        at_exit 
          (fun () ->
             try 
               BaseLog.unregister ocamlbuild_clean_ev extra_cli
             with _ ->
               ())
      end

(** Run ocamlbuild, unregister all clean events *)
let run_ocamlbuild args extra_argv =
  BaseExec.run (ocamlbuild ()) (fix_args args extra_argv);
  (* Remove any clean event, we must run it again *)
  List.iter
    (fun (e, d) -> BaseLog.unregister e d)
    (BaseLog.filter [ocamlbuild_clean_ev])

(** Determine real build directory *)
let build_dir extra_argv =
  let rec search_args dir =
    function
      | "-build-dir" :: dir :: tl ->
          search_args dir tl
      | _ :: tl ->
          search_args dir tl
      | [] -> 
          dir
  in
    search_args "_build" (fix_args [] extra_argv)
