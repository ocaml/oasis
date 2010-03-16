
(** Manipulate filename
    @author Sylvain Le Gall
  *)

open Filename

(** Concat elements of a path
  *)
let make =
  function 
    | [] ->
        invalid_arg "BaseFilename.make"
    | hd :: tl ->
        List.fold_left Filename.concat hd tl

(** Convert a unix filename into host filename
  *)
let of_unix ufn =
  make
    (List.map
       (function
          | "."  -> current_dir_name
          | ".." -> parent_dir_name
          | p    -> p)
       (OASISUtils.split '/' ufn))
