
(** Parse value of OASIS file
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

(* Check that string match a Str.regexp *)
let str_regexp regexp error (_ : ctxt) str = 
  if Str.string_match regexp str 0 && 
     (Str.match_beginning ()) = 0 &&
     (Str.match_end ()) = (String.length str) then
      str
  else
    failwith 
      (Printf.sprintf "String '%s' is not a %s" str error)
;;

(** Check that we have an URL *)
let url = 
  str_regexp
    (Str.regexp "http://[a-zA-Z0-9\\./]+")
    "URL"
;;

(** Check that we have a version number *)
let version =
  str_regexp
    (Str.regexp "[0-9]+\\(\\.[0-9]+\\)*")
    "version"
;;

(** Check that we a (C) copyright *)
let copyright _ str =
  if Str.string_match 
       (Str.regexp "\\((c)\\|(C)\\) * [0-9]+\\(-[0-9]+\\)?,? .*") 
       str 0 then
    str
  else
    failwith 
      (Printf.sprintf
         "Copyright must follow the convention \
         '(C) 2008-2009 J.R. Hacker', here it is '%s'"
         str)
;;

(** String is not empty *)
let string_not_empty _ str =
  if str <> "" then
    str
  else
    failwith "Expecting not empty string"
;;

(** File exists *)
let file_exists ctxt fn = 
  if not (Sys.file_exists (Filename.concat ctxt.srcdir fn)) then
    failwith 
      (Printf.sprintf "File '%s' doesn't exist" fn)
  else
    fn
;;

(** Directory exists *)
let directory_exists ctxt fn =
  let rfn =
    Filename.concat ctxt.srcdir fn
  in
    if (Sys.file_exists rfn) && (Sys.is_directory rfn) then
      fn
    else
      failwith 
        (Printf.sprintf "Directory '%s' doesn't exist" fn)
;;

(** Convert string to boolean *)
let boolean _ str =
  match String.lowercase str with
    | "true"  -> true
    | "false" -> false
    | _ ->
        failwith 
          (Printf.sprintf 
             "Boolean value must be 'true' \
             or 'false', not '%s'"
             str)
;;

(** Convert a comma separated string into list *)
let comma_separated _ =
  let separator =
    Str.regexp " *, *"
  in
    Str.split separator 
;;

(** Convert string to build depends *)
let build_depends ctxt str =
  let white_spaces =
    "[ \t]*"
  in
  let not_white_spaces =
    "[^ \t]*"
   in
  let strip_whitespace =
    Str.regexp (white_spaces^"\\("^not_white_spaces^"\\)"^white_spaces)
  in
  let split_version =
    Str.regexp ("\\("^not_white_spaces^"\\)"^
                white_spaces^
                "("^white_spaces^"\\(.*\\)"^white_spaces^")")
  in
  let parse_one str =
    if Str.string_match split_version str 0 then
      (Str.matched_group 1 str), 
      Some (Str.matched_group 2 str)
    else if Str.string_match strip_whitespace str 0 then
      (Str.matched_group 1 str),
      None
    else
      str, 
      None
  in
    List.map
      parse_one
      (comma_separated ctxt str)
;;

(** Convert string to module lists *)
let modules ctxt str =
  let whitespaces =
    Str.regexp "[ \t]+"
  in
  let lst = 
    Str.split whitespaces str
  in
    List.map 
      (str_regexp 
         (Str.regexp "[A-Z][A-Za-z0-9_]*")
         "module"
         ctxt)
      lst
;;

(** Convert string to URL *)
let categories _ str = 
  [str]
;;

(** Optional value *)
let opt f ctxt str =
  Some (f ctxt str)
;;
