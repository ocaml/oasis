
(** AST types
    @author Sylvain Le Gall
  *)

open OASISTypes;;

(** Context for building/checking AST 
  *)
type ctxt = 
    {
      oasisfn:      filename;    (* Location of OASIS file *)
      srcdir:       dirname;     (* Toplevel source directory *)
      cond:         expr option; (* Condition for this context
                                    can be used with certain
                                    field.
                                    By default ETrue (no condition)
                                  *)
      valid_flags:  name list; (* Which flags are valid *)
    }
;;

(** Abstract Syntax Tree *)
type stmt =
  | SField of name * string
  | SIfThenElse of expr * stmt * stmt
  | SBlock of stmt list
;;

type top_stmt = 
  | TSLibrary of name * stmt
  | TSExecutable of name * stmt
  | TSFlag of name * stmt
  | TSSourceRepository of name * stmt
  | TSStmt of stmt
  | TSBlock of top_stmt list
;; 

