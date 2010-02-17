
(** Manipulate section 
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

let section_fields _ _ =
  fun nm data ->
    {
      cs_name = nm;
      cs_data = data;
    }
