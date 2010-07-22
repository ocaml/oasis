
(** Unix path manipulation 
  
  The filename and dirname used in '_oasis' file and {!OASISTypes.package} are
  always encoded as Unix path. They are changed when using it on the target 
  system.

  @author Sylvain Le Gall
  *)

type unix_filename = string

(** '.' on Unix. *)
val current_dir_name : unix_filename

(** '..' on Unix. *)
val parent_dir_name : unix_filename

(** [concat fn1 fn2] Concatenate fn1 and fn2, i.e. [fn1^'/'^fn2]. *)
val concat : unix_filename -> unix_filename -> unix_filename

(** [make lst] Concatenate all filename components of [lst]. *)
val make : unix_filename list -> unix_filename

(** [dirname fn] Return directory name of [fn] or [current_dir_name] if no
    directory name is defined.
  *)
val dirname : unix_filename -> unix_filename

(** [basename fn] Return filename without its directory name.
  *)
val basename : unix_filename -> unix_filename

(** [chop_extension fn] Remove the last part of the filename, after a '.',
    return [fn] if there is no extension.
  *)
val chop_extension : unix_filename -> unix_filename
