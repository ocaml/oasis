
(** Global context for messages and i18n
    @author Sylvain Le Gall
  *)

type level =
  [ `Debug
  | `Info 
  | `Warning
  | `Error]

type t = 
  { 
    verbose : bool; 
    (** Display info, warnings and errors messages. *)

    debug : bool;   
    (** Display also debug messages. *)

    ignore_plugins : bool;
    (** Don't use plugins *)

    printf: level -> string -> unit; 
    (** Redirect output *)
  }

(** Default context *)
val default: t ref

(** Command line arguments to change {!default}. *)
val args: unit -> (string * Arg.spec * string) list

(** Quiet context. *)
val quiet : t
