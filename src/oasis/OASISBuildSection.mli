(** Build section
    @author Sylvain Le Gall
  *)

(** Compute the order of section building, taking into account
    build dependencies between sections. {b Not exported}.
  *)
val build_order : OASISTypes.package -> OASISTypes.section list 

(** Compute a map between sections and its build depends.
    The build depends contains only libraries. {b Not exported}.
  *)
val transitive_build_depends :
  OASISTypes.package -> OASISTypes.dependency list OASISSection.MapSection.t
