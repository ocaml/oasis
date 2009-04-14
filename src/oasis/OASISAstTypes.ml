
(** AST types
    @author Sylvain Le Gall
  *)

open OASISTypes;;

(** Context for building/checking AST 
  *)
type ctxt = 
    {
      oasisfn:      filename;
      srcdir:       dirname;
      valid_tests:  name list;
      valid_flags:  name list;
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
  | TSStmt of stmt
  | TSBlock of top_stmt list
;; 

