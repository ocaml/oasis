
(** Manipulate section 
    @author Sylvain Le Gall
  *)

open OASISTypes 

(** {2 Manipulate a section} *)

(** Extract generic information 
  *)
val section_kind_common :  section -> section_kind * common_section

(** Common section of a section
  *)
val section_common : section -> common_section

(** Set the common part of a section 
  *)
val section_common_set : common_section -> section -> section

(** Key used to identify section
  *)
val section_id : section -> section_kind * name

(** Convert a section to a short string (only informations returned by
    {!section_id}
  *)
val string_of_section : section -> string

(** Find a section 
  *)
val section_find : section_kind * name -> section list -> section

(** {2 Containers for sections} *)

(** Comparable section, we only rely on section_id
   for comparison. {b Not exported}
  *)
module CSection: 
sig 
  type t = section
  val compare: section -> section -> int
  val equal: section -> section -> bool
  val hash: section -> int
end

(** Map using CSection. {b Not exported} *)
module MapSection: Map.S with type key = CSection.t 

(** Set using CSection. {b Not exported} *)
module SetSection: Set.S with type elt = CSection.t

