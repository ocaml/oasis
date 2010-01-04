
(** Parse value of OASIS file
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open CommonGettext;;

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
let str_regexp regexp error str = 
  if Str.string_match regexp str 0 && 
     (Str.match_beginning ()) = 0 &&
     (Str.match_end ()) = (String.length str) then
      str
  else
    failwith 
      (Printf.sprintf 
         (f_ "String '%s' is not a %s")
         str 
         error)
;;

(** Check that we have an URL *)
let url str = 
  str_regexp
    StdRegexp.url
    (s_ "URL")
    str
;;

(** Check that we have a version number *)
let version str =
  str_regexp
    StdRegexp.version
    (s_ "version")
    str
;;

(** Check that we have a version constraint *)
let version_constraint str = 
  str_regexp 
    StdRegexp.version_constraint
    (s_ "version constraint")
    str
;;

(** Check that we a (C) copyright *)
let copyright str =
  if Str.string_match StdRegexp.copyright str 0 then
    str
  else
    failwith 
      (Printf.sprintf
         (f_ "Copyright must follow the convention \
              '(C) 2008-2009 J.R. Hacker', here it is '%s'")
         str)
;;

(** String is not empty *)
let string_not_empty str =
  if str <> "" then
    str
  else
    failwith (s_ "Expecting not empty string")
;;

(** File *)
let file fn = 
  fn
;;

(** Directory *)
let directory fn =
  fn
;;

(** Convert a comma separated string into list *)
let comma_separated =
  let separator =
    Str.regexp " *, *"
  in
    Str.split separator 
;;

(** Split a string that with an optional value: "e1 (e2)" *)
let optional_parent value_parse option_parse option_default =
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
                (strip_whitespace s1)
            in
            let e2 =
              option_parse
                (strip_whitespace s2)
            in
              e1, e2
          )
        else 
          value_parse str, option_default
      )
;;

(** Optional value *)
let opt f str =
  Some (f str)
;;

(** Convert string to build depends *)
let build_depends str =
  List.rev_map 
    (fun (pkg, ver_constr_opt) -> 
       FindlibPackage (pkg, ver_constr_opt))
    (List.rev_map
       (optional_parent 
          string_not_empty 
          (opt version_constraint)
          None)
       (comma_separated str))
;;

(** Convert string to data files specification *)
let data_files str =
  List.map
    (optional_parent
       (* TODO: match constraint for wildcard *)
       string_not_empty
       string_not_empty
       "$datarootdir/$pkg_name")
    (comma_separated str)
;;

(** Convert string to module list *)
let modules str =
  List.map 
    (str_regexp 
       StdRegexp.modul
       (s_ "module"))
    (comma_separated str)
;;

(** Convert string to file list *)
let files = 
  comma_separated
;;

(** Convert string to URL *)
let categories str = 
  List.map
    url
    (comma_separated str)
;;

(** Choices 
  *)
let choices nm lst str =
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
         (f_ "Unknown %s %S (possible: %s)")
         nm
         str
         (String.concat ", " (List.map fst lst)))
;;

(** Compilation types
  *)
let compiled_object str =
  choices
    (s_ "compiled object")
    ["byte", Byte; "native", Native; "best", Best]
    str
;;

(** Convert string to boolean *)
let boolean str =
  choices 
    (s_ "boolean")
    ["true", true; "false", false]
    str
;;

