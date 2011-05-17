open OASISTypes

let section_kind_common = 
  function
    | Library (cs, _, _) -> 
        `Library, cs
    | Executable (cs, _, _) ->
        `Executable, cs
    | Flag (cs, _) ->
        `Flag, cs
    | SrcRepo (cs, _) ->
        `SrcRepo, cs
    | Test (cs, _) ->
        `Test, cs
    | Doc (cs, _) ->
        `Doc, cs

let section_common sct =
  snd (section_kind_common sct)

let section_common_set cs =
  function
    | Library (_, bs, lib)     -> Library (cs, bs, lib)
    | Executable (_, bs, exec) -> Executable (cs, bs, exec)
    | Flag (_, flg)            -> Flag (cs, flg)
    | SrcRepo (_, src_repo)    -> SrcRepo (cs, src_repo)
    | Test (_, tst)            -> Test (cs, tst)
    | Doc (_, doc)             -> Doc (cs, doc)

(** Key used to identify section
  *)
let section_id sct = 
  let k, cs = 
    section_kind_common sct
  in
    k, cs.cs_name

let string_of_section sct =
  let k, nm =
    section_id sct
  in
    (match k with
       | `Library    -> "library" 
       | `Executable -> "executable"
       | `Flag       -> "flag"
       | `SrcRepo    -> "src repository"
       | `Test       -> "test"
       | `Doc        -> "doc")
    ^" "^nm

(* END EXPORT *)

let section_find id scts =
  List.find
    (fun sct -> id = section_id sct)
    scts

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

