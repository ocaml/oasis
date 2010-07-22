
(** Common types for OASISVersion parser
    @author Sylvain Le Gall
  *)

type t =  
  | VCGt of string
  | VCGe of string
  | VCEq of string
  | VCLt of string
  | VCLe of string
  | VCOr  of t * t
  | VCAnd of t * t
