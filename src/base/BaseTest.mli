
(** Run tests
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [test lst pkg extra_args] Run all tests. [lst] elements are tuples
    [(f, cs, test)], apply in turn [f pkg (cs, test) extra_args] and 
    collect their results. Combine all results to give a percentage of
    failure.
  *)
val test :
  ((package -> 
      common_section * test -> 
      arg array -> 
      float) *
   common_section * 
   test) list -> 
  package -> 
  arg array -> 
  unit
