open Printf

module String =
struct
  include String

  let strip s =
    Str.replace_first (Str.regexp "^[ \t\n]+") ""
      (Str.replace_first (Str.regexp "[ \t\n]+$") "" s)

  let rchop s = String.sub s 0 (String.length s - 1)
end

module List =
struct
  include List

  let filter_map f l =
    let rec loop acc = function
        [] -> List.rev acc
      | hd::tl ->
          match f hd with Some x -> loop (x :: acc) tl | None -> loop acc tl
    in loop [] l
end

type field =
    [ `Field of string * string | `IfBlock of string * field list * field list ]
(* TODO: use condition type, parse condition properly *)

type section_contents =
    [ field | `Section of string * string option * section_contents list ]

type section = [ `Section of string * string option * section_contents list ]

type spec = section_contents list

type line = {
  lineno : int;
  indent : int;
  has_tabs : bool;
  contents : string;
}

let (@@) f x = f x
let (@@@) f g x = f (g x)
let (|>) x f = f x

let split_lines s = Str.split (Str.regexp "\n") s

let lines str =
  let valid_line l = match l.contents with
      "" -> false
    | s when s.[0] = '#' -> false
    | _ -> true in
  let i = ref 0 in
  let lineinfo s =
    incr i;
    match Str.string_match (Str.regexp "\\([ \t]*\\)\\(.*\\)") s 0 with
        false -> assert false
      | true ->
          let pref = Str.matched_group 1 s
          and contents = String.strip @@ Str.matched_group 2 s in
            {
              lineno = !i; indent = String.length pref;
              has_tabs = String.contains pref '\t'; contents = contents ;
            }
  in split_lines str |> List.map lineinfo |> List.filter valid_line

module SMap = Map.Make(struct type t = string let compare = String.compare end)
let failwithfmt fmt = kprintf failwith fmt

type schema = { sections : schema SMap.t; fields : string list }

let section sects fields =
  {
    sections = List.fold_left (fun m (n, s) -> SMap.add n s m) SMap.empty sects;
    fields = fields;
  }

let split_on_space s = Str.split (Str.regexp "[ \t\n]+") s
let skip_fst_token s =
  let off = String.index s ' ' + 1 in
  String.strip @@ String.sub s off (String.length s - off)
let fst_token s = List.hd @@ split_on_space s

let rec parse section_desc indent schema =
  let parse_field_lines indent lines =
    let subs_empty_line = function
        "." -> ""
      | l -> l in
    let text acc = String.concat "\n" (List.rev_map String.strip acc) in
    let rec loop acc = function
        [] -> (text acc, [])
      | l::tl as ls ->
          if l.indent < indent then (text acc, ls)
          else loop (subs_empty_line l.contents :: acc) tl
    in loop [] lines in

  let parse_field l tl s rest =
    let fieldname = String.rchop s in
      if not (List.mem fieldname schema.fields) then
        failwithfmt "Unknown field %S in %S at line %d"
          fieldname section_desc l.lineno;
      begin match rest with
          [] ->
            begin match tl with
                [] -> failwithfmt "Unterminated field %S in %S at line %d"
                        fieldname section_desc l.lineno;
              | fst::_ as tl ->
                  let data, tl = parse_field_lines fst.indent tl in
                    (`Field (fieldname, data), tl)
            end
        | _ -> (`Field (fieldname, skip_fst_token l.contents), tl)
      end in

  let parse_section section_desc l tl =
    let section, options = match split_on_space l.contents with
        [s] -> (s, None)
      | s::opts -> (s, Some (String.concat " " opts))
      | [] -> failwithfmt "cannot parse (sub)section header in %S at line %d"
                section_desc l.lineno
    in try
      let subschema = SMap.find section schema.sections in
      let fields, tl = parse (section_desc ^ "::" ^ section) l.indent subschema tl in
        (`Section (section, options, fields), tl)
    with Not_found ->
      failwithfmt "Unknown section %S in %S at line %d"
        section section_desc l.lineno in

  let (>->) x g =
    let f, tl = x in
    let fs, tl = g tl in (f :: fs, tl)

  in function
    [] -> ([], [])
  | l::tl as lines ->
      if l.indent <= indent then ([], lines)
      else match split_on_space l.contents with
          s::rest when s.[String.length s - 1] = ':' -> (* field *)
            parse_field l tl s rest >-> parse section_desc indent schema
        | "if"::_ ->
            let ifindent = l.indent in
            let cond = skip_fst_token l.contents in
            let subfields, tl = parse section_desc ifindent schema tl in
            (match tl with
                | l::tl' when fst_token l.contents = "else" ->
                    let elsesubs, tl = parse section_desc l.indent schema tl' in
                      (`IfBlock (cond, subfields, elsesubs), tl)
                | _ -> (`IfBlock (cond, subfields, []), tl)) >->
            parse section_desc indent schema
        | _ -> (* section *)
            parse_section section_desc l tl >-> parse section_desc indent schema

let parse schema text =
  let tree, remainder = parse "" (-1) schema (lines text) in match remainder with
      [] -> tree
    | l::_ -> failwithfmt "EOF expected before line %d (%S)" l.lineno l.contents


type env = value SMap.t
and value = [`String of string | `Bool of bool]

let empty_environment () = SMap.empty

let eval_cond env s = true (* TODO *)

let get_sections stype =
  List.filter (function `Section (t, _, _) when t = stype -> true | _ -> false)

let get_field_aux f x path tree env =
  let rec collect_values field l =
    List.concat @@ List.filter_map
      (function
           `Field (field', v) when field' = field -> Some [v]
         | `IfBlock (cond, iftrue, iffalse) ->
             let sub = if eval_cond env cond then iftrue else iffalse in
               Some (collect_values field sub)
         | _ -> None) l in
  let rec loop ts = function
      [] -> x
    | [field] -> List.fold_left f x @@ collect_values field ts
    | section::tl -> match split_on_space section with
          [sname] ->
            begin try
              match List.find (function `Section (s, _, _) -> s = sname | _ -> false) ts with
                  `Section (_, _, ts) -> loop ts tl
                | _ -> x
            with Not_found -> x end
        | [stype; sname] ->
            begin try
              match List.find
                      (function `Section (t, name, _) -> t = stype && name = Some sname
                         | _ -> false)
                      ts with
                | `Section (_, _, ts) -> loop ts tl
                | _ -> x
            with Not_found -> x end
        | _ -> failwithfmt "Invalid field path %S." path
  in loop tree (Str.split (Str.regexp "/") path)

let get_field_values path tree env =
  get_field_aux (fun l x -> x :: l) [] path tree env |> List.rev

let get_field_value path tree env =
  try
    List.hd @@ get_field_values path tree env
  with _ -> failwithfmt "Field missing: %S" path

let sample =
"
Name: Stuff
Version: 1.2.3.0
Copyright: 2003-2006, J. R. Hacker
Homepage: http://foo.com
Author: J. R. Hacker <hacker@example.com>
Maintainer: J. R. Hacker <hacker@example.com>
License-File: LICENSE
License: LGPL-link-exn

# blank lines are ignored, as are comments

Synopsis: some stuff
Description:
  This is a rather short desc.
  .
  (with an empty line)

Library src/stuff
  Modules: A B C
"

type package = {
  name : string;
  license : license;
  license_file : string;
  copyright : string;
  maintainer : string;
  author : string;
  homepage : string;
  synopsis : string;
  description : string;
  (* category : string; *)
  build_depends : dependency list;
  libraries : lib_info list;
}

and lib_info = {
  lib_buildable : bool;
  lib_path : string;
  lib_modules : string list option;
}

and license = [
  `GPL | `LGPL | `LGPL_link_exn | `BSD3 | `BSD4 | `PublicDomain | `AllRightsReserved
  | `Other of string
]

and dependency = string * version

and version = { version_branch : int list; version_tags : string list }

let schema =
  let libsection = section [] ["Modules"; "Ocaml-Flags"]
  in section
    ["Library", libsection ]
    [
      "Name"; "Version"; "Copyright"; "License"; "License-File";
      "Author"; "Maintainer"; "Homepage"; "Synopsis"; "Description";
      "Category"; "Build-Type";
    ]

let license_of_string s = match String.lowercase s with
    "gpl" -> `GPL
  | "lgpl" -> `LGPL
  | "lgpl-link-exn" -> `LGPL_link_exn
  | "bsd3" -> `BSD3
  | "bsd4" -> `BSD4
  | "public-domain" -> `PublicDomain
  | "all-rights-reserved" -> `AllRightsReserved
  | s -> `Other s

let package_of_string env s =
  let tree = parse schema s in
  let get ?(tree = tree) path = get_field_value path tree env in
  let get_default ?(tree = tree) f default path =
    try f @@ get_field_value path tree env with _ -> default in
  (* let getl ?(tree = tree) path = get_field_values path tree env in *)
  let libs =
    get_sections "Library" tree |>
    List.filter_map
      (function
           `Section(_, path, fields) ->
             Some {
               lib_buildable = get_default ~tree:fields bool_of_string true "Buildable";
               lib_path =
                 begin match path with
                     Some p -> p
                   | None -> failwith "Missing pathname in Library section."
                 end;
               lib_modules =
                 begin match get_default ~tree:fields (fun x -> Some x) None "Modules" with
                     None -> None
                   | Some text ->
                       Some begin
                         split_lines text |> List.map split_on_space |> List.concat
                       end
                 end;
             }
         | _ -> None)
  in
  {
    name = get "Name";
    license = license_of_string @@ get "License";
    license_file = get "License-File";
    copyright = get "Copyright";
    maintainer = get "Maintainer";
    author = get "Author";
    homepage = get "Homepage";
    synopsis = get "Synopsis";
    description = get "Description";
    (* category = get "Category"; *)
    build_depends = []; (* FIXME *)
    libraries = libs;
  }

(*  LGPL with linking exception
 *  Copyright (C) 2008 Stéphane Glondu
 *  Laboratoire PPS - CNRS Université Paris Diderot
 * *)
let default_myocamlbuild = "
open Ocamlbuild_plugin

let packages =
  [
  %%PACKAGES%%
  ]

let ocamlfind x = S[A\"ocamlfind\"; A x]

dispatch begin function
  | Before_options ->
      (* Options.include_dirs := \"src\" :: !Options.include_dirs; *)
      Options.make_links := false;
      (* Override default commands by ocamlfind ones *)
      Options.ocamlc := ocamlfind \"ocamlc\";
      Options.ocamlopt := ocamlfind \"ocamlopt\";
      Options.ocamldep := ocamlfind \"ocamldep\";
      (* There seems to be no other way to specify ocamldoc... *)
      Options.ocamldoc := ocamlfind \"ocamldoc\";

  | After_rules ->
      (* For each ocamlfind package one inject the -package option
       when compiling, computing dependencies, generating
       documentation and linking. *)
      List.iter begin fun pkg ->
        flag [\"ocaml\"; \"compile\"; \"pkg_\"^pkg] & S[A\"-package\"; A pkg];
        flag [\"ocaml\"; \"ocamldep\"; \"pkg_\"^pkg] & S[A\"-package\"; A pkg];
        flag [\"ocaml\"; \"doc\"; \"pkg_\"^pkg] & S[A\"-package\"; A pkg];
        flag [\"ocaml\"; \"link\"; \"pkg_\"^pkg] & S[A\"-package\"; A pkg];
      end packages;

      (* The default \"thread\" tag is not compatible with ocamlfind.
       Indeed, the default rules add the \"threads.cma\" or
       \"threads.cmxa\" options when using this tag. When using the
       \"-linkpkg\" option with ocamlfind, this module will then be
       added twice on the command line.

       To solve this, one approach is to add the \"-thread\" option
       when using the \"threads\" package using the previous
       plugin. *)
      flag [\"ocaml\"; \"pkg_threads\"; \"compile\"] & S[A \"-thread\"];
      flag [\"ocaml\"; \"pkg_threads\"; \"link\"] & S[A \"-thread\"];

  | _ -> ()
end
"
