
(** Build documentation
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [doc lst pkg extra_args] Build all documents. [lst] elements are tuples
    [(f, cs, doc)], apply in turn [f pkg (cs, doc) extra_args].
  *)
val doc :
  ((package -> 
      common_section * doc -> 
      arg array -> 
      unit) *
   common_section * 
   doc) list -> 
  package -> 
  arg array ->
  unit
