
(** Version comparisons

    This module handles versions and version comparators. A version is a string
    of the form "1.0.0". We compare integer and non-integer parts between to 
    version to order them. Version comparators defined relations to a set
    of version. E.g. ">= 1.0.0" is a version comparator and defines all version
    above "1.0.0", including "1.0.0".

    The version comparison is done using Debian policy for version:
    http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version

    @author Sylvain Le Gall
  *)

(** {2 Version} *)

type s = string

type t

(** Compare versions.
  *)
val version_compare: t -> t -> int

(** Convert a string to version.
  *)
val version_of_string: string -> t

(** Convert a version to string.
  *)
val string_of_version: t -> string

(** Version number value. {b Not exported}.
  *)
val value: t OASISValues.t

(** Dump [ODN.t]. {b Not exported}.
  *)
val odn_of_t: t -> ODN.t

(** Remove the last part of a version, after the last '.'. I.e. 0.2.0~alpha1 ->
    0.2.
  *)
val chop: t -> t

(** {2 Version comparator} *)

type comparator = 
  | VGreater of t
  | VGreaterEqual of t
  | VEqual of t
  | VLesser of t
  | VLesserEqual of t
  | VOr of  comparator * comparator
  | VAnd of comparator * comparator

(** Apply version comparator expression.
  *)
val comparator_apply: t -> comparator -> bool

(** Convert a comparator to string.
  *)
val string_of_comparator: comparator -> string

(** Convert a comparator to variable name.
  *)
val varname_of_comparator: comparator -> string

(** Convert a string to comparator. {b Not exported}.
  *)
val comparator_of_string: string -> comparator

(** Simplify comparator, if possible. {b Not exported}.
  *)
val comparator_reduce: comparator -> comparator

(** Check that we have a version constraint. {b Not exported}. 
  *)
val comparator_value: comparator OASISValues.t

(** Dump [ODN.t]. {b Not exported}. 
  *)
val odn_of_comparator:  comparator -> ODN.t
