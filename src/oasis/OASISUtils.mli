
(** Various utilities 
    @author Sylvain Le Gall
  *)

(** {2 Map} *)

module MapString: Map.S with type key = String.t

(** Convert a string association list to a map. *)
val map_string_of_assoc : (string * 'a) list -> 'a MapString.t

(** {2 Set} *)

(** Set for String. 
  *)
module SetString: Set.S with type elt = String.t

(** Add a string list to an existing Set. *)
val set_string_add_list : SetString.t -> SetString.elt list -> SetString.t

(** Convert a string list to a Set. *)
val set_string_of_list : SetString.elt list -> SetString.t

(** {2 Hashtable} *)

(** Caseless string hashtable
  *)
module HashStringCsl: Hashtbl.S with type key = String.t

(** {2 Variable name} *)

(** [varname_of_string ~hyphen:c s] Transform a string [s] into a variable name, 
    following this convention: no digit at the beginning, lowercase, only a-z
    and 0-9 chars. Whenever there is a problem, use an hyphen char.
  *)
val varname_of_string : ?hyphen:char -> string -> string

(** [varname_concat ~hyphen p s] Concat variable name, removing hyphen at end
    of [p] and at beginning of [s].
  *)
val varname_concat : ?hyphen:char -> string -> string -> string

(** [is_varname str] Check that the string [str] is a valid varname. See
    {!varname_of_string} for definition.
  *)
val is_varname: string -> bool

(** {2 Fail with Printf.sprintf} *)

(** These functions are combinations of [failwith] and [Printf.sprintf]. The
    numbering scheme is unfortunate but required to have a polymorphic functions.
    Without setting the number of arguments, the return of [failwith] prevents
    type inference, because this function doesn't return.

    Example: [failwithf2 "Cannot do %s because of %d" str i]
  *)

val failwithf1 : ('a -> string, unit, string) format -> 'a -> 'b
val failwithf2 : ('a -> 'b -> string, unit, string) format -> 'a -> 'b -> 'c
val failwithf3 : ('a -> 'b -> 'c -> string, unit, string) format -> 'a -> 'b -> 'c -> 'd
val failwithf4 : ('a -> 'b -> 'c -> 'd -> string, unit, string) format -> 'a -> 'b -> 'c -> 'd -> 'e
val failwithf5 : ('a -> 'b -> 'c -> 'd -> 'e -> string, unit, string) format -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f

(** {2 String} *)

(** [split c s] Split the string [s] at char [c]. It doesn't include the
    separator.
  *)
val split : char -> string -> string list

(** Caseless compare function
  *)
val compare_csl : string -> string -> int

(** Split a list using ',' as separator. {b Not exported}
  *)
val split_comma : string -> string list

(** Split a string containing '(...)' optionally. {b Not exported} 
  *)
val split_optional_parentheses : string -> string * (string option)
