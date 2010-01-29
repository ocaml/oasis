
(** Extra function for Format module
    
   @author Sylvain Le Gall
  *)

open Format

(** Print a string considering ' ' as Format space
  *)
let pp_print_string_spaced fmt str =
  String.iter
    (function
       | ' ' -> Format.pp_print_space fmt ()
       | c -> Format.pp_print_char fmt c)
    str

(** Print a list of element
  *)
let pp_print_list pp_elem lst_sep fmt =
  function
    | [] ->
        ()
    | hd :: tl ->
        pp_elem fmt hd;
        List.iter
          (fun e ->
             fprintf fmt lst_sep;
             pp_elem fmt e)
          tl

