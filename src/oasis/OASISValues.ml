
(** Parse, print and check values of OASIS file
    @author Sylvain Le Gall
  *)

open OASISTypes
open CommonGettext
open ExtString

(** The value exist but there is no easy way to represent it
  *)
exception Not_printable

module StdRegexp = 
struct 
  let r = Str.regexp

  let url       = r "http://[a-zA-Z0-9\\./_?&;=-]+"
  let copyright = r "\\((c)\\|(C)\\) * [0-9]+\\(-[0-9]+\\)?,? .*" 
  let modul     = r "[A-Z][A-Za-z0-9_]*"
end

(* Check that string match a Str.regexp *)
let regexp regexp error = 
  {
    parse = 
      (fun str -> 
         if Str.string_match regexp str 0 && 
            (Str.match_beginning ()) = 0 &&
            (Str.match_end ()) = (String.length str) then
           str
         else
           failwith 
             (Printf.sprintf 
                (f_ "String '%s' is not a %s")
                str 
                (error ())));
    print = (fun s -> s);
  }

(** Check that we have an URL *)
let url = 
  regexp
    StdRegexp.url
    (fun () -> s_ "URL")

(** Check that we a (C) copyright *)
let copyright =
  {
    parse = 
      (fun str ->
         if Str.string_match StdRegexp.copyright str 0 then
           str
         else
           failwith 
             (Printf.sprintf
                (f_ "Copyright must follow the convention \
                     '(C) 2008-2009 J.R. Hacker', here it is '%s'")
                str));
    print = (fun s -> s);
  }


(** String *)
let string =
  { 
    parse = (fun s -> s);
    print = (fun s -> s);
  }

(** String is not empty *)
let string_not_empty =
  {
    parse =
      (fun str ->
         if str <> "" then
           str
         else
           failwith (s_ "Expecting not empty string"));
    print = (fun s -> s);
  }

(** File *)
let file = 
  string

(** Directory *)
let directory =
  string

(** Convert a dot separated string into list, don't strip whitespace *)
let dot_separated value =
  {
    parse =
      (fun s ->
         List.map
           value.parse
           (String.nsplit
              s
              "."));
    print =
      (fun lst ->
         String.concat "." 
           (List.map 
              value.print
              lst));
  }

(** Convert a comma separated string into list, strip whitespace *)
let comma_separated value =
  { 
    parse = 
      (fun s ->
         List.map 
           (fun s -> 
              value.parse 
                (String.strip s))
           (String.nsplit 
              s 
              ","));
    print = 
      (fun lst ->
         String.concat ", "
           (List.map 
              value.print
              lst));
  }

(** Check that we have a version number *)
let version =
  {
    parse = OASISVersion.version_of_string;
    print = OASISVersion.string_of_version;
  }

(** Check that we have a version constraint *)
let version_comparator = 
  {
    parse = OASISVersion.comparator_of_string;
    print = OASISVersion.string_of_comparator;
  }

(** Split a string that with an optional value: "e1 (e2)" *)
let with_optional_parentheses main_value optional_value =
  let split_parentheses =
    Str.regexp "\\([^(]*\\)(\\([^)]*\\))"
  in
    {
      parse = 
        (fun str ->
           if Str.string_match split_parentheses str 0 then
             begin
               let s1, s2 = 
                 Str.matched_group 1 str,
                 Str.matched_group 2 str
               in
               let e1 = 
                 main_value.parse
                   (String.strip s1)
               in
               let e2 =
                 optional_value.parse
                   (String.strip s2)
               in
                 e1, Some e2
             end
           else 
             begin
               main_value.parse str, None
             end);
      print =
        (function
           | v, None ->
               main_value.print v
           | v, Some opt ->
               Printf.sprintf "%s (%s)"
                 (main_value.print v)
                 (optional_value.print opt));
    }

(** Optional value *)
let opt value =
  {
    parse = (fun str -> Some (value.parse str));
    print =
      (function
         | Some v -> value.print v
         | None -> raise Not_printable);
  }

(** Convert string to build depends *)
let build_depends =
  let base_value = 
    comma_separated 
      (with_optional_parentheses
         string_not_empty
         version_comparator)
  in
    {
      parse = 
        (fun str ->
           List.map 
             (fun (pkg, ver_constr_opt) -> 
                FindlibPackage (pkg, ver_constr_opt))
             (base_value.parse str));
      print =
        (fun lst ->
           base_value.print
             (List.map 
                (function 
                   | FindlibPackage (nm, ver) -> (nm, ver)
                   | InternalLibrary nm -> (nm, None))
                lst));
    }

(** Convert string to data files specification *)
let data_files =
  comma_separated
    (with_optional_parentheses
       (* TODO: these two strings are in fact "expendable strings" i.e. that
        * can contain $xxx, we need to check their correctness 
        *)
       string_not_empty
       string_not_empty)

(** Convert string to module list *)
let modules =
  comma_separated
    (regexp 
       StdRegexp.modul
       (fun () -> s_ "module"))

(** Convert string to file list *)
let files = 
  comma_separated file

(** Convert string to URL *)
let categories = 
  comma_separated url 

(** Choices 
  *)
let choices nm lst =
  {
    parse = 
      (fun str ->
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
                (nm ())
                str
                (String.concat ", " (List.map fst lst))));
    print =
      (fun v ->
         try
           List.assoc
             v
             (List.map
                (fun (s, v) -> v, s)
                lst)
         with Not_found ->
           failwith 
             (Printf.sprintf
                (f_ "Unexpected abstract choice value for %s")
                (nm ())));
  }

(** Compilation types
  *)
let compiled_object =
  choices
    (fun () -> s_ "compiled object")
    ["byte", Byte; "native", Native; "best", Best]

(** Convert string to boolean *)
let boolean =
  choices 
    (fun () -> s_ "boolean")
    ["true", true; "false", false]

(** Findlib package name 
  *)
let pkgname =
  {
    parse = 
      (fun s ->
         if String.contains s '.' then
           failwith "Findlib package name cannot contain '.'"
         else
           s);
    print =
      (fun s -> s);
  }

(** Internal library
  *)
let internal_library =
  (* TODO: check that the library really exists *)
  string

(** Command line 
  *)
let command_line = 
  { 
    parse = 
      (fun s ->
         let cli =
           List.filter 
             (fun s -> s <> "")
             (String.nsplit s " ")
         in
           match cli with 
             | cmd :: args ->
                 cmd, args
             | [] ->
                 failwith 
                   (Printf.sprintf
                      (f_ "Commande line '%s' is invalid")
                      s));
    print = 
      (fun (cmd, args) -> 
         String.concat " " (cmd :: args))
  }
