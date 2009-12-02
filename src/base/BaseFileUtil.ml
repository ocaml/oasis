
(** {1 File operation (install, which...)
  *)

(** Find a file among all provided alternatives
  *)
let find_file paths exts = 

  (* Cardinal product of two list *)
  let ( * ) lst1 lst2 = 
    List.flatten 
      (List.map 
         (fun a -> 
            List.map 
              (fun b -> a,b) 
              lst2) 
         lst1)
  in

  let rec combined_paths lst = 
    match lst with
      | p1 :: p2 :: tl ->
          let acc = 
            (List.map 
               (fun (a,b) -> Filename.concat a b) 
               (p1 * p2))
          in
            combined_paths (acc :: tl)
      | [e] ->
          e
      | [] ->
          []
  in

  let alternatives =
    List.map 
      (fun (p,e) -> 
         if String.length e > 0 && e.[0] <> '.' then
           p ^ "." ^ e
         else
           p ^ e) 
      ((combined_paths paths) * exts)
  in
    try 
      List.find Sys.file_exists alternatives
    with Not_found ->
      failwith 
        (Printf.sprintf 
           "Cannot find any of the files: %s"
           (String.concat ", " 
              (List.map 
                 (Printf.sprintf "%S")
                 alternatives)))
;;

(** Find real filename of an executable
  *)
let which prg =
  let path_sep =
    match Sys.os_type with 
      | "Win32" ->
          ';'
      | _ ->
          ':'
  in
  let path_lst =
    BaseUtils.split 
      path_sep 
      (Sys.getenv "PATH")
  in
  let exec_ext = 
    match Sys.os_type with 
      | "Win32" ->
          "" 
          :: 
          (BaseUtils.split 
             path_sep 
             (Sys.getenv "PATHEXT"))
      | _ ->
          [""]
  in
    find_file [path_lst; [prg]] exec_ext;  
;;

(** Copy a file 
  *)
let cp src tgt = 
  match Sys.os_type with 
    | "Win32" ->
        BaseExec.run "copy" [src; tgt]
    | _ ->
        BaseExec.run "cp" [src; tgt]
;;

(** Create a directory
  *)
let mkdir tgt =
  match Sys.os_type with 
    | "Win32" ->
        BaseExec.run "md" [tgt]
    | _ ->
        BaseExec.run "mkdir" [tgt]
;;

(** Remove a directory
  *)
let rmdir tgt =
  if Sys.readdir tgt = [||] then
    (
      match Sys.os_type with 
        | "Win32" ->
            BaseExec.run "rd" [tgt]
        | _ ->
            BaseExec.run "rm" ["-r"; tgt]
    )
;;
