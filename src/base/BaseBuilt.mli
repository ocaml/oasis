(** Register files built to be installed
    @author Sylvain Le Gall
  *)

open OASISTypes

type t =
  | BExec    (* Executable. *)
  | BExecLib (* Library coming with executable. *)
  | BLib     (* Library. *)
  | BDoc     (* Document. *)

(** Register files built. Each files built is provided as a list 
    of alternatives. At least one alternative file should exist 
    when registering and we only register existing ones.
  *)
val register : t -> name -> host_filename list list -> unit

(** Unregister all files built. *)
val unregister : t -> name -> unit

(** Fold-left files built, filter existing
    and non-existing files.
 *)
val fold : t -> name -> ('a -> host_filename -> 'a) -> 'a -> 'a

(** Check if a library/doc/exec has been built. 
  *)
val is_built : t -> name -> bool

(** [of_executable loc_fn (cs, bs, exec)] Compute at the same time events 
    that should be registered by {!register} and data returned by 
    {!OASISExecutable.unix_exec_is}. Use [loc_fn], if generated files 
    are moved to a directory different from sources (e.g. in directory
    "_build").
  *)
val of_executable :
  (unix_filename -> host_filename) ->
  common_section * build_section *
  executable ->
  (t * name * host_filename list list) list * unix_filename * unix_filename option

(** [of_library loc_fn (cs, bs, lib)]  Same as {!of_executable}, but
    using {!OASISLibrary.generated_unix_files}.
  *)
val of_library :
  (unix_filename -> host_filename) ->
  common_section * build_section * library ->
  (t * name * host_filename list list) list * unix_filename list list
