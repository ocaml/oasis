
(** License definition
    
    This module allows to manipulate DEP-5 style license.

    @author Sylvain Le Gall
    @see <http://dep.debian.net/deps/dep5/> DEP-5
  *)

type url = string

(** Valid licenses exception.
  *)
type license_exception = 
  | OCamlLinkingException
  | OtherException of url
  
(** Valid licenses.
  *)
type t =
  | Proprietary
  | BSD3
  | BSD4
  | GPL
  | LGPL
  | QPL
  | CeCILL
  | CeCILLB
  | CeCILLC
  | LicenseWithVersion of t * OASISVersion.t
  | LicenseWithLaterVersion of t * OASISVersion.t
  | LicenseWithException of t * license_exception
  | OtherLicense of url

(** Convert a license to string. {b Not exported}.
  *)
val to_string : t -> string

(** License value. {b Not exported}.
  *)
val value : t OASISValues.t

(** Display help. {b Not exported}.
  *)
val help : unit -> string

(** All available license, their short name, their long name, and compatible 
    versions. {b Not exported}.
  *)
val license_data: unit -> (t * string * string * string list) list

(** All available license exception, their name, and compatible license.
  {b Not exported}.
  *)
val license_exception_data: unit -> (license_exception * string * t list) list

(** Dump [ODN.t]. {b Not exported}.
  *)
val odn_of_t : t -> ODN.t
