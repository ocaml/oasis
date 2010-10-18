(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open OASISGettext
open OASISUtils
open ExtString

exception Not_printable
exception Not_combinable

type 'a t =
    {
      parse:  ctxt:OASISContext.t -> string -> 'a;
      update: 'a -> 'a -> 'a;
      print:  'a -> string;
    }

let update_fail _ _ =
  raise Not_combinable

let blackbox =
  {
    parse  = 
      (fun ~ctxt s -> 
         failwithf1
           (f_ "Blackbox type cannot be set to the value '%s'")
           s);
    update = update_fail;
    print  = (fun _ -> raise Not_printable);
  }

module StdRegexp = 
struct 
  let r = Pcre.regexp

  let url_scheme = "[A-Za-z][A-Za-z0-9+\\-\\.]*"
  let url_path   = "(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(/|/([\\w#!:.?+=&%@!\\-/]))?"

  let url       = r (url_scheme ^ "://" ^ url_path)
  let copyright = r "\\((c|C)\\) *\\d+(-\\d+)?,? .*" 
  let modul     = r "[A-Z][A-Za-z0-9_]*"
end

let regexp regexp nm = 
  {
    parse = 
      (fun ~ctxt str -> 
         try 
           let substrs = 
             Pcre.exec ~rex:regexp str
           in
           let str_matched = 
             Pcre.get_substring substrs 0
           in
             if str_matched = str then
               str
             else
               failwithf3
                 (f_ "Only substring '%s' of '%s' is a %s")
                 str_matched
                 str
                 (nm ())
         with Not_found ->
           failwithf2
             (f_ "String '%s' is not a %s")
             str 
             (nm ()));
    update = update_fail;
    print = (fun s -> s);
  }

let url = 
  regexp
    StdRegexp.url
    (fun () -> s_ "URL")

let copyright =
  {
    parse = 
      (fun ~ctxt str ->
         if Pcre.pmatch ~rex:StdRegexp.copyright str then
           str
         else
           failwithf1
             (f_ "Copyright must follow the convention \
                  '(C) 2008-2009 J.R. Hacker', here it is '%s'")
             str);
    update = update_fail;
    print = (fun s -> s);
  }

let string =
  { 
    parse =  (fun ~ctxt s -> s);
    update = (fun s1 s2 -> s1^" "^s2); 
    print =  (fun s -> s);
  }

let string_not_empty =
  {
    parse =
      (fun ~ctxt str ->
         if str <> "" then
           str
         else
           failwith (s_ "Expecting not empty string"));
    update = (fun s1 s2 ->s1^" "^s2);
    print = (fun s -> s);
  }

let file = 
  {string_not_empty with update = update_fail}

let file_glob =
  {string_not_empty with update = update_fail}

let directory =
  {string_not_empty with update = update_fail}


let expandable value =
  (* TODO: check expandable value *)
  value

let dot_separated value =
  {
    parse =
      (fun ~ctxt s ->
         List.map
           (value.parse ~ctxt)
           (String.nsplit
              s
              "."));
    update = 
      List.append;
    print =
      (fun lst ->
         String.concat "." 
           (List.map 
              value.print
              lst));
  }

let comma_separated value =
  { 
    parse = 
      (fun ~ctxt s ->
         List.map 
           (fun s -> value.parse ~ctxt s)
           (split_comma s));
    update = 
      List.append;
    print = 
      (fun lst ->
         String.concat ", "
           (List.map 
              value.print
              lst));
  }

let space_separated = 
  {
    parse = 
      (fun ~ctxt s ->
         List.filter 
           (fun s -> s <> "")
           (String.nsplit s " "));
    update = 
      List.append;
    print =
      (fun lst ->
         String.concat " " lst);
  }

let with_optional_parentheses main_value optional_value =
  {
    parse = 
      (fun ~ctxt str ->
         match split_optional_parentheses str with 
           | e1, Some e2 ->
               main_value.parse ~ctxt e1,
               Some (optional_value.parse ~ctxt e2)
           | e1, None ->
               main_value.parse ~ctxt e1,
               None);
    update = update_fail;
    print =
      (function
         | v, None ->
             main_value.print v
         | v, Some opt ->
             Printf.sprintf "%s (%s)"
               (main_value.print v)
               (optional_value.print opt));
  }

let opt value =
  {
    parse = (fun ~ctxt str -> Some (value.parse ~ctxt str));
    update = update_fail;
    print =
      (function
         | Some v -> value.print v
         | None -> raise Not_printable);
  }

let modules =
  let base_value = 
    regexp 
      StdRegexp.modul
      (fun () -> s_ "module")
  in
    comma_separated
      {
        parse = 
         (fun ~ctxt s ->
            let path = 
              OASISUnixPath.dirname s
            in
            let modul = 
              OASISUnixPath.basename s
            in
              if String.contains path ' ' then
                failwithf1
                  (f_ "Module path '%s' must not contain a ' '")
                  s;
              OASISUnixPath.concat
                path
                (base_value.parse ~ctxt modul));
        update = update_fail;
        print  = (fun s -> s);
      }

let files = 
  comma_separated file

let categories = 
  comma_separated url 

let choices nm lst =
  {
    parse = 
      (fun ~ctxt str ->
         try 
           List.assoc 
             (String.lowercase str)
             (List.map 
               (fun (k, v) ->
                  String.lowercase k, v)
                  lst)
         with Not_found ->
           failwithf3
             (f_ "Unknown %s %S (possible: %s)")
             (nm ()) str
             (String.concat ", " (List.map fst lst)));
    update = update_fail;
    print =
      (fun v ->
         try
           List.assoc
             v
             (List.map
                (fun (s, v) -> v, s)
                lst)
         with Not_found ->
           failwithf1
             (f_ "Unexpected abstract choice value for %s")
             (nm ()));
  }

let boolean =
  choices 
    (fun () -> s_ "boolean")
    ["true", true; "false", false]

let findlib_name =
  {
    parse = 
      (fun ~ctxt s ->
         if s = "" then
           failwith (s_ "Empty string is not a valid findlib package")
         else if String.contains s '"' || String.contains s '.' then
           failwith (s_ "Findlib package name cannot contain '.' or '\"'")
         else
           s);
    update = update_fail;
    print = (fun s -> s);
  }

let findlib_full = 
  {
    parse = 
      (fun ~ctxt s -> 
         let cpnts = 
           String.nsplit s "."
         in
           List.iter 
             (fun cpnt -> 
                let _s : string = 
                  findlib_name.parse ~ctxt cpnt
                in 
                  ())
             cpnts;
           s);
    update = update_fail;
    print = (fun s -> s);
  }

let internal_library =
  (* TODO: check that the library really exists *)
  {string with update = update_fail}

let command_line = 
  let split_expandable str =

    (* Add a single char to accumulator *)
    let rec addchr c =
      function
        | Some b, _ as acc -> 
            Buffer.add_char b c;
            acc
        | None, l -> 
            let b =
              Buffer.create 13
            in
              addchr c (Some b, l)
    in

    (* Add a separator that will end the previous
     * token or do nothing if already separated
     *)
    let addsep =
      function
        | Some b, l -> 
            None, (Buffer.contents b) :: l
        | None, l ->
            None, l
    in

    (* Split the list of char into a list of token
     * taking care of matching $( ... ) and ${ ... }
     *)
    let rec lookup_closing oc cc acc =
      function
        | c :: tl ->
            let acc = 
              addchr c acc
            in
              if c = oc then
                begin
                  let acc, tl =
                    lookup_closing oc cc acc tl
                  in
                    lookup_closing oc cc acc tl
                end
              else if c = cc then
                begin
                  acc, tl
                end
              else
                begin
                  lookup_closing oc cc acc tl 
                end
        | [] ->
            failwithf1
              (f_ "'%s' contains unbalanced curly braces")
              str
    in
    let rec lookup_dollar acc = 
      function 
        | '$' :: ('(' as c) :: tl
        | '$' :: ('{' as c) :: tl -> 
            begin
              let acc, tl = 
                lookup_closing 
                  c (if c = '(' then ')' else '}')
                  (addchr c (addchr '$' acc))
                  tl
              in
                lookup_dollar acc tl
            end
        | ' ' :: tl -> 
            lookup_dollar (addsep acc) tl
        | c :: tl -> 
            lookup_dollar (addchr c acc) tl
        | [] ->
            begin
              let l = 
                match acc with
                  | Some b, l -> Buffer.contents b :: l
                  | None, l -> l
              in
                List.rev l
            end
    in

    (* Transform string into list
     *)
    let lst =
      let rl = ref []
      in
        String.iter (fun c -> rl := c :: !rl) str;
        List.rev !rl
    in

      lookup_dollar (None, []) lst
  in
    
    { 
      parse = 
        (fun ~ctxt s ->
           match split_expandable s with 
             | cmd :: args ->
                 cmd, args
             | [] ->
                 failwithf1 (f_ "Commande line '%s' is invalid") s);
      update =
        (fun (cmd, args1) (arg2, args3) ->
           (cmd, args1 @ (arg2 :: args3)));
      print = 
        (fun (cmd, args) -> 
           space_separated.print (cmd :: args))
    }

