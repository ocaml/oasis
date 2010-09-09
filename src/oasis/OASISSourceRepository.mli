
(** SourceRepository section
    @author Sylvain Le Gall
  *)

open OASISTypes 

(** Schema for the section. {b Not exported}.
  *)
val schema : (common_section * source_repository) OASISSchema.t
