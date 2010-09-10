
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

(** DEP-5 license.
  *)
type license_dep_5 =
    { 
      license:    license;
      exceptions: license_exception list;
      version:    license_version;
    }

(** Extended DEP-5 license.
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
val to_string : t -> string

(** Convert a license to string. {b Not exported}.
  *)
val string_of_license: license -> string

(** Convert a license exception to string. {b Not exported}.
  *)
val string_of_license_exception: license_exception -> string

(** License value. {b Not exported}.
  *)
val value : t OASISValues.t

(** Display help. {b Not exported}.
  *)
val help : unit -> string

(** All available license, their short name, their long name, and compatible 
    versions. {b Not exported}.
  *)
val license_data: unit -> (license * license_data) list

(** All available license exception, their name, and compatible license.
  {b Not exported}.
  *)
val license_exception_data: unit -> (license_exception * license_exception_data) list

(** Dump [ODN.t]. {b Not exported}.
  *)
val odn_of_t : t -> ODN.t

(** {2 License definitions}
   
    {b No licenses are exported.}
  *)

val proprietary: license
val apache: license
val artistic: license
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
val public_domain: license

val ocaml_linking_exception: license_exception
