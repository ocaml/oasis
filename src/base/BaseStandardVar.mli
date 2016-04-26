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


(** Standard variables
    @author Sylvain Le Gall
*)


open OASISTypes


(** {2 Programs} *)


val ocamlfind:  unit -> host_filename
val ocamlc:     unit -> host_filename
val ocamlopt:   unit -> host_filename
val ocamlbuild: unit -> host_filename
val flexlink:   unit -> host_filename
val flexdll_version: unit -> string


(** {2 Variables from OASIS package}

    See {!OASISTypes.package} for more information.

*)


val pkg_name:    unit -> name
val pkg_version: unit -> OASISVersion.s


(** {2 ocamlc config variables} *)


val os_type:                  unit -> string
val system:                   unit -> string
val architecture:             unit -> string
val ccomp_type:               unit -> string
val ocaml_version:            unit -> string
val standard_library_default: unit -> host_dirname
val standard_library:         unit -> host_dirname
val standard_runtime:         unit -> host_filename
val bytecomp_c_compiler:      unit -> string
val native_c_compiler:        unit -> string
val model:                    unit -> string
val ext_obj:                  unit -> string
val ext_asm:                  unit -> string
val ext_lib:                  unit -> string
val ext_dll:                  unit -> string
val default_executable_name:  unit -> string
val systhread_supported:      unit -> string


(** {2 Paths}

    See {{:http://www.gnu.org/prep/standards/html_node/Directory-Variables.html} GNU standards}.

*)


val prefix:         unit -> host_dirname
val exec_prefix:    unit -> host_dirname
val bindir:         unit -> host_dirname
val sbindir:        unit -> host_dirname
val libexecdir:     unit -> host_dirname
val sysconfdir:     unit -> host_dirname
val sharedstatedir: unit -> host_dirname
val localstatedir:  unit -> host_dirname
val libdir:         unit -> host_dirname
val datarootdir:    unit -> host_dirname
val datadir:        unit -> host_dirname
val infodir:        unit -> host_dirname
val localedir:      unit -> host_dirname
val mandir:         unit -> host_dirname
val docdir:         unit -> host_dirname
val htmldir:        unit -> host_dirname
val dvidir:         unit -> host_dirname
val pdfdir:         unit -> host_dirname
val psdir:          unit -> host_dirname
val destdir:        unit -> host_dirname


(** {2 Various} *)


(** Findlib version.
*)
val findlib_version: unit -> OASISVersion.s


(** Check that the platform is a native platform (can compile native
    exec/library).
*)
val is_native: unit -> string


(** Compute the default suffix for program (e.g. '.exe' on Win32).
*)
val ext_program: unit -> string


(** Host command to delete a file.
*)
val rm: unit -> string


(** Host command to delete a directory.
*)
val rmdir: unit -> string


(** Compile in debug mode.
*)
val debug: unit -> string


(** Compile in profile mode.
*)
val profile: unit -> string


(** Run tests.
*)
val tests: unit -> string


(** Compile docs.
*)
val docs: unit -> string


(** Support for .cmxs.
*)
val native_dynlink: unit -> string


(** Initialize some variables.
*)
val init: OASISTypes.package -> unit
