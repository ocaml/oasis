
(** AST types
    @author Sylvain Le Gall
  *)

open OASISTypes

(** Context for parsing and checking AST *)
type ctxt =
    {
      cond:         expr option; (* Condition for this context
                                    can be used with certain
                                    field.
                                    By default ETrue (no condition)
                                  *)
      valid_flags:  name list; (* Which flags are valid *)
    }

(** Abstract Syntax Tree *)
type stmt =
  | SField of name * string
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

