
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

type section_kind =
  | KLibrary 
  | KExecutable
  | KFlag
  | KSrcRepo
  | KTest

(** Key used to identify section
  *)
let section_id sct = 
  let k, cs = 
    match sct with
      | Library (cs, _, _) -> 
          KLibrary, cs
      | Executable (cs, _, _) ->
          KExecutable, cs
      | Flag (cs, _) ->
          KFlag, cs
      | SrcRepo (cs, _) ->
          KSrcRepo, cs
      | Test (cs, _) ->
          KTest, cs
  in
    k, cs.cs_name

let string_of_section sct =
  let k, nm =
    section_id sct
  in
    (match k with
       | KLibrary    -> "library" 
       | KExecutable -> "executable"
       | KFlag       -> "flag"
       | KSrcRepo    -> "src repository"
       | KTest       -> "test")
    ^" "^nm

(** {2 Module for full section} *)

(** Comparable section, we only rely on section_id
   for comparison
  *)
module CSection =
struct
  type t = section

  let id = section_id

  let compare t1 t2 = 
    compare (id t1) (id t2)
    
  let equal t1 t2 =
    (id t1) = (id t2)

  let hash t =
    Hashtbl.hash (id t)
end

module MapSection = Map.Make(CSection)
module SetSection = Set.Make(CSection)

(** {2 Module for id-only section} *)

module CIdSection = 
struct 
  type t = section_kind * name
  let compare = compare
end

module MapSectionId = Map.Make(CIdSection)
module SetSectionId = Set.Make(CIdSection)

(** Convert a MapSection.t into a MapSectionId.t
  *)
let map_section_id mp =
  MapSection.fold
    (fun k v mp ->
       MapSectionId.add
         (section_id k)
         v
         mp)
    mp
    MapSectionId.empty

