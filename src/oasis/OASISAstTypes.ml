
(** AST types
    @author Sylvain Le Gall
  *)

open OASISTypes

(** Context for parsing and checking AST *)
type ctxt =
    {
      (* Current condition for conditional fields. *)
      cond: expr option; 

      (* Valid flags *)
      valid_flags:  name list;

      (* Combine values rather than setting it, when
         setting field values
       *)
      append: bool; 
    }

(** Abstract Syntax Tree *)
type field_op =
  | FSet of string
  | FAdd of string
  | FEval of expr

type stmt =
  | SField of name * field_op
  | SIfThenElse of expr * stmt * stmt
  | SBlock of stmt list

type top_stmt = 
  | TSLibrary of name * stmt
  | TSExecutable of name * stmt
  | TSFlag of name * stmt
  | TSSourceRepository of name * stmt
  | TSTest of name * stmt
  | TSStmt of stmt
  | TSBlock of top_stmt list

type ver_cmp_t =
  | VCGt of string
  | VCGe of string
  | VCEq of string
  | VCLt of string
  | VCLe of string
  | VCOr  of ver_cmp_t * ver_cmp_t
  | VCAnd of ver_cmp_t * ver_cmp_t

