(** i18n functions
  
    This module is really bind to ocaml-gettext library if gettext has been selected
    when compiling the project. All these functions {b are exported} but their are bound
    to {b dummy functions} in this case (i.e. not ocaml-gettext).

    @author Sylvain Le Gall
    @see <http://forge.ocamlcore.org/projects/ocaml-gettext> OCaml Gettext project
  *)

(** Do nothing, but register it for translation *)
val ns_: string -> string

(** Translate a string. *)
val s_ : string -> string

(** Translate a format string. *)
val f_ : ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4

(** [fn_ fmt_singular fmt_plural n] Translate a plural string using either
    [fmt_singular] or [fmt_plural], depending of the plural status of number
    [n] in the target language.
  *)
val fn_ : ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4 ->  int -> ('a, 'b, 'c, 'd) format4

(** Gettext initialization. *)
val init: (string * string option * string option) list  
