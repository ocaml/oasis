
(** Remove trailing whitespace *)
let strip_whitespace str =
  let strlen = 
    String.length str
  in
  let is_whitespace =
    function 
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false
  in 
  let skip_beg =
    let idx =
      ref 0
    in
      while !idx < strlen && is_whitespace str.[!idx] do 
        incr idx 
      done;
      !idx
  in
  let skip_end =
    let idx = 
      ref ((String.length str) - 1)
    in
      while !idx >= 0 && is_whitespace str.[!idx] do
        decr idx
      done;
      !idx
  in
    if skip_beg <= skip_end then
      String.sub str skip_beg (skip_end - skip_beg + 1)
    else
      ""
;;

open Format;;

let pp_list pp_elem lst_sep fmt =
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
;;

let pp_record_open fmt () = 
  fprintf fmt "@[{@[<hv>@,"
;;

let pp_record_field fmt nm pp_value value =
    fprintf fmt "@[%s = %a@]" nm pp_value value
;;

let pp_record_sep fmt () = 
  fprintf fmt ";@ "
;;

let pp_record_close fmt () =
  fprintf fmt "@]@,}@]"
;;

let pp_print_ostring fmt str =
  fprintf fmt "%S" str
;;
