
(** Common utilities for testing
    @author Sylvain Le Gall
  *)

module MapString = Map.Make(String);;
module SetString = Set.Make(String);;

type context =
    {
      dbug:         bool;
      has_ocamlopt: bool;
    }
;;

let in_data fn =
  Filename.concat "data" fn
;;
