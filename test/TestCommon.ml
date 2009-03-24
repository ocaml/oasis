
(** Common utilities for testing
    @author Sylvain Le Gall
  *)

type context =
    {
      dbug: bool;
    }
;;

let in_data fn =
  Filename.concat "data" fn
;;
