
(** Parse value of OASIS file
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

module StdRegexp = 
struct 
  let r = Str.regexp

  let s_version = "[0-9]+\\(\\.[0-9]+\\)*"

  let url       = r "http://[a-zA-Z0-9\\./_?&;=-]+"
  let version   = r s_version
  let copyright = r "\\((c)\\|(C)\\) * [0-9]+\\(-[0-9]+\\)?,? .*" 
  let modul     = r "[A-Z][A-Za-z0-9_]*"

  let version_constraint = 
    r 
      ("\\(\\(\\(>=?\\|<=?\\|=\\) *"^s_version^"\\|&&\\|||\\) *\\)*")
end
;;

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
    StdRegexp.url
    "URL"
;;

(** Check that we have a version number *)
let version =
  str_regexp
    StdRegexp.version
    "version"
;;

let version_constraint = 
  str_regexp 
    StdRegexp.version_constraint
    "version constraint"
;;

(** Check that we a (C) copyright *)
let copyright _ str =
  if Str.string_match StdRegexp.copyright str 0 then
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

(** Split a string that with an optional value: "e1 (e2)" *)
let optional_parent value_parse option_parse option_default ctxt =
  let white_spaces =
    "[ \t]*"
  in
  let not_white_spaces =
    "[^ \t]*"
   in
  let strip_whitespace =
    Str.global_replace 
      (Str.regexp 
         (Printf.sprintf "^%s|%s$" white_spaces white_spaces))
      ""
  in
  let split_version =
    Str.regexp ("\\("^not_white_spaces^"\\)"^
                white_spaces^
                "("^white_spaces^"\\(.*\\)"^white_spaces^")")
  in
    fun str ->
      (
        if Str.string_match split_version str 0 then
          (
            let s1, s2 = 
              Str.matched_group 1 str,
              Str.matched_group 2 str
            in
            let e1 = 
              value_parse 
                ctxt 
                (strip_whitespace s1)
            in
            let e2 =
              option_parse
                ctxt
                (strip_whitespace s2)
            in
              e1, e2
          )
        else 
          value_parse ctxt str, option_default
      )
;;

(** Optional value *)
let opt f ctxt str =
  Some (f ctxt str)
;;

(** Convert string to build depends *)
let build_depends ctxt str =
  List.rev_map 
    (fun (pkg, ver_constr_opt) -> 
       FindlibPackage (pkg, ver_constr_opt))
    (List.rev_map
       (optional_parent 
          string_not_empty 
          (opt version_constraint)
          None
          ctxt)
       (comma_separated ctxt str))
;;

(** Convert string to data files specification *)
let data_files ctxt str =
  List.map
    (optional_parent
       (* TODO: match constraint for wildcard *)
       string_not_empty
       string_not_empty
       "$datarootdir/$pkg_name"
       ctxt)
    (comma_separated ctxt str)
;;

(** Convert string to module list *)
let modules ctxt str =
  List.map 
    (str_regexp 
       StdRegexp.modul
       "module"
       ctxt)
    (comma_separated ctxt str)
;;

(** Convert string to file list *)
let files = 
  comma_separated
;;

(** Convert string to URL *)
let categories ctxt str = 
  List.map
    (url ctxt)
    (comma_separated ctxt str)
;;

(** Choices 
  *)
let choices nm lst _ str =
  try 
    List.assoc 
      (String.lowercase str)
      (List.map 
        (fun (k, v) ->
           String.lowercase k, v)
           lst)
  with Not_found ->
    failwith 
      (Printf.sprintf 
         "Unknown %s %S (possible: %s)"
         nm
         str
         (String.concat ", " (List.map fst lst)))
;;

(** Compilation types
  *)
let compiled_object ctxt str =
  choices
    "compiled object"
    ["byte", Byte; "native", Native; "best", Best]
    ctxt
    str
;;


