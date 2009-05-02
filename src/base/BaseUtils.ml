
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

