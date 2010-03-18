
(** Manipulate Unix style path
    @author Sylvain Le Gall
  *)

let current_dir_name = "."

let parent_dir_name = ".."

let concat f1 f2 = 
  if f1 = current_dir_name then
    f2
  else if f2 = current_dir_name then
    f1
  else
    f1^"/"^f2

let make =
  function
    | hd :: tl ->
        List.fold_left
          (fun f p -> concat f p)
          hd
          tl
    | [] ->
        invalid_arg "OASISUnixPath.make"

let dirname f =
  try
    String.sub f 0 (String.rindex f '/')
  with Not_found ->
    current_dir_name

let chop_extension f =
  try 
    let last_dot =
      String.rindex f '.'
    in
    let sub =
      String.sub f 0 last_dot
    in
      try 
        let last_slash =
          String.rindex f '/'
        in
          if last_slash < last_dot then
            sub
          else
            f
      with Not_found ->
        sub

  with Not_found ->
    f

