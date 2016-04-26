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


(** License definition

    This module allows to manipulate DEP-5 style license.

    @author Sylvain Le Gall
    @see <http://dep.debian.net/deps/dep5/> DEP-5
*)


(** Valid licenses
*)
type license


(** Valid license exceptions.
*)
type license_exception


(** License version.
*)
type license_version =
  | Version of OASISVersion.t
  | VersionOrLater of OASISVersion.t
  | NoVersion


(** DEP-5 license, basic.
*)
type license_dep_5_unit =
  {
    license:    license;
    excption:   license_exception option;
    version:    license_version;
  }


(** DEP-5 license, complex.
*)
type license_dep_5 =
  | DEP5Unit of license_dep_5_unit
  | DEP5Or of license_dep_5 list
  | DEP5And of license_dep_5 list


(** OASIS supported type of license.
*)
type t =
  | DEP5License of license_dep_5
  | OtherLicense of string (* URL *)


(** Extra data about license {b Not exported}
*)
type license_data =
  {
    long_name: string;
    (** Expanded name of the license. *)

    versions:  OASISVersion.t list;
    (** Standard versions of the license. *)

    note:      string option;
    (** Extra information about the license. *)

    deprecated: string option;
    (** Deprecated alternative. *)
  }


(** Extra data about license exception {b Not exported}
*)
type license_exception_data =
  {
    explanation: string;
    (** Purpose of the exception. *)

    licenses:    license list;
    (** Compatible licenses with the exception. *)
  }


(** Convert a DEP-5 license to string. {b Not exported}.
*)
val to_string: t -> string


(** Convert a DEP-5 license to a legal disclaimer for the product.
    {b Not exported}.
*)
val legal_disclaimer: string -> t -> string


(** Convert a license to string. {b Not exported}.
*)
val string_of_license: license -> string


(** Convert a license exception to string. {b Not exported}.
*)
val string_of_license_exception: license_exception -> string


(** License value. {b Not exported}.
*)
val value: t OASISValues.t


(** Choices for quickstart question. {b Not exported}.
*)
val choices: unit -> t list


(** All available license, their short name, their long name, and compatible
    versions. {b Not exported}.
*)
val license_data: unit -> (license * license_data) list


(** All available license exception, their name, and compatible license.
    {b Not exported}.
*)
val license_exception_data:
  unit -> (license_exception * license_exception_data) list


(** Dump [ODN.t]. {b Not exported}.
*)
val odn_of_t: t -> ODN.t


(** {2 License definitions}

    {b No licenses are exported.}
*)


val proprietary: license
val apache: license
val artistic: license
val bsd2: license
val bsd3: license
val bsd4: license
val cecill: license
val cecillb: license
val cecillc: license
val freebsd: license
val isc: license
val cc_by: license
val cc_by_sa: license
val cc_by_nd: license
val cc_by_nc: license
val cc_by_nc_sa: license
val cc_by_nc_nd: license
val cc0: license
val cddl: license
val cpl: license
val eiffel: license
val expat: license
val gpl: license
val lgpl: license
val agpl: license
val gfdl: license
val gfdl_niv: license
val lppl: license
val mpl: license
val perl: license
val psf: license
val qpl: license
val w3c_software: license
val zlib: license
val zope: license
val mit: license
val wtfpl: license
val public_domain: license


val ocaml_linking_exception: license_exception
