
(** Manipulate host filenames
    @author Sylvain Le Gall
  *)

open OASISTypes

(** See {!OASISUnixPath}. *)
module Unix:  
sig
  val concat : unix_filename -> unix_filename -> unix_filename
  val make : unix_filename list -> unix_filename
  val dirname : unix_filename -> unix_filename
  val basename : unix_filename -> unix_filename
  val chop_extension : unix_filename -> unix_filename
end

(** Create a filename out of its components.
  *)
val make : host_filename list -> host_filename

(** Convert a unix filename into host filename.
  *)
val of_unix : unix_filename -> host_filename
