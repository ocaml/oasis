
(** Manipulate filename
    @author Sylvain Le Gall
  *)

open Filename

module Unix = OASISUnixPath

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
       (fun p ->
          if p = Unix.current_dir_name then
            current_dir_name
          else if p = Unix.parent_dir_name then
            parent_dir_name
          else
            p)
       (OASISUtils.split '/' ufn))

