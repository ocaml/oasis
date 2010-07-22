
(** Generate 'setup.ml' and other files
    @author Sylvain Le Gall
  *)

open OASISTypes

(** Restore generated files, when [generate] has been called with 
    [~restore:true]. {b Not exported}.
  *)
val restore : ?msg:OASISContext.t -> unit -> unit

(** Generate 'setup.ml' file and the rest of the build system.
    {b Not exported}.
  *)
val generate :
  ?msg:OASISContext.t ->
  restore:bool ->
  backup:bool ->
  dev:bool ->
  setup_fn:host_filename ->
  ?oasis_exec:string ->
  package -> OASISFileTemplate.file_generate_change list
