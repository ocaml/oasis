
(** Extra functions for Format
    
   @author Sylvain Le Gall
  *)

(** Print a string considering ' ' as Format space.
  *)
val pp_print_string_spaced : Format.formatter -> string -> unit

(** [pp_print_list pp_elem sep fmt lst] Print the list [lst] of elements
    using [pp_elem] for each element and separate them by [sep].
  *)
val pp_print_list :
  (Format.formatter -> 'a -> unit) ->
  ('b, Format.formatter, unit) format -> Format.formatter -> 'a list -> unit
