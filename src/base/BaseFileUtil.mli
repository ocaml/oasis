(** File operations
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [find_file paths exts] Find a file among all provided [paths], trying
    various extensiosn [exts]. Return the first combination of [paths]
    and [exts].
  *)
val find_file : host_filename list list -> string list -> host_filename

(** Find real filename of an executable.
  *)
val which : host_filename -> host_filename

(** Copy a file.
  *)
val cp : host_filename -> host_filename -> unit

(** Create a directory.
  *)
val mkdir : host_filename -> unit

(** [mkdir_parent f tgt] Create a directory and its parent, call f with 
    directory name created, in order.
  *)
val mkdir_parent : (host_filename -> 'a) -> host_filename -> unit

(** Remove a directory.
  *)
val rmdir : host_filename -> unit

(** Expand a filename containing '*.ext' into corresponding
    real files.
  *)
val glob : string -> host_filename list
