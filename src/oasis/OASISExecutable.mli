
(** Executable section
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [unix_exec_is (cs, bs, exec) is_native ext_dll suffix_program] Compute the
    filename of the real executable, with full unix path. Also return executable
    library, if one exists (it happens when building bytecode executable with C
    stubs).
 *)
val unix_exec_is :
  common_section * build_section * executable ->
  (unit -> bool) ->
  (unit -> string) -> (unit -> string) -> 
  unix_filename * unix_filename option

(** Schema for the section. {b Not exported}.
  *)
val schema : (common_section * build_section * executable) OASISSchema.t
