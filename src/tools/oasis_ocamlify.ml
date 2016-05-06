#!/usr/bin/env ocaml

(********************************************************************************)
(*  ocamlify: include files in OCaml code                                       *)
(*                                                                              *)
(*  Copyright (C) 2009-2010, OCamlCore SARL                                     *)
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

open Genlex;;

type var_type = 
  | VarString
  | VarStringList

let to_ocaml_string str = 
  let ocaml_str = 
    Printf.sprintf "%S" str
  in
  if String.length ocaml_str >= 2 && 
     ocaml_str.[0] = '"' && 
     ocaml_str.[(String.length ocaml_str) - 1] = '"' then
    String.sub ocaml_str 1 ((String.length ocaml_str) - 2) 
  else
    str

(** Create output file containing variable *)
let ocamlify fd var_lst =
  let output_var fd vartype varname file =
    let fpf str =
      Printf.fprintf fd str
    in
    let iter_line f =
      let fi = 
        open_in file
      in
      try
        while true do
          f (input_line fi)
        done
      with End_of_file ->
        close_in fi
    in
    fpf "(* Include %s *)\n" file;
    fpf "let %s = \n" varname;
    match vartype with 
      | VarString ->
        fpf "  \"\\\n";
        (
          iter_line 
            (fun str ->
               let ocaml_str =
                 to_ocaml_string str
               in
               let fst_part, lst_part =
                 if String.length ocaml_str > 0 then
                   let last_str = 
                     (String.sub ocaml_str 1 ((String.length ocaml_str) - 1))
                   in
                   match ocaml_str.[0] with 
                     | ' '  -> "\\ ", last_str
                     | '\n' -> "\\n", last_str
                     | '\t' -> "\\t", last_str
                     | '\r' -> "\\r", last_str
                     | _ -> "", ocaml_str
                 else
                   "", ocaml_str
               in
               fpf "  %s%s\\n\\\n"
                 fst_part 
                 lst_part
            );
          fpf "  \"\n";
          fpf ";;\n";
        )
      | VarStringList ->
        (
          fpf "  [\n";
          iter_line (fpf "    %S;\n");
          fpf "  ]\n";
        )
  in
  let rec output_all_var fd lst =
    match lst with 
      | [] ->
        ()
      | (vartype, varname, file) :: tl ->
        output_var fd vartype varname file;
        output_all_var fd tl
  in
  output_all_var fd var_lst

(** Print dependencies *)
let depends fd var_lst fn =
  List.iter 
    (fun (_, _, fnd) -> 
       ( match fn with 
         | None ->
           ()
         | Some fn ->
           output_string fd (fn^": "));
       output_string fd (fnd^"\n"))
    var_lst

(** Read configuration file *)
let parse_file = 
  let lexer =
    make_lexer ["VarString"; "VarStringList"]
  in
  let rec to_list acc str =
    try
      to_list ((Stream.next str) :: acc) str
    with Stream.Failure ->
      List.rev acc
  in
  let error fn s = 
    failwith ("Don't know what to do with '"^s^"' in file '"^fn^"'")
  in
  let rebase_fn fn_base fn =
    (* TODO: FilePath.Unix ! *)
    if Filename.is_relative fn then
      (Filename.dirname fn_base)^"/"^fn
    else
      fn
  in
  let rec analyze fn acc lst =
    match lst with 
      | Kwd "VarString" :: Ident var :: String fni :: tl ->
        analyze 
          fn 
          ((VarString, var, (rebase_fn fn fni)) :: acc)
          tl
      | Kwd "VarStringList" :: Ident var :: String fni :: tl ->
        analyze 
          fn 
          ((VarStringList, var, (rebase_fn fn fni)) :: acc)
          tl
      | Kwd s :: _ | Ident s :: _ | String s :: _ ->
        error fn s
      | Int i :: _ ->
        error fn (string_of_int i)
      | Float f :: _ ->
        error fn (string_of_float f)
      | Char c :: _ ->
        error fn (String.make 1 c)
      | [] ->
        acc
  in
  fun fn acc ->
    let fd =
      open_in fn
    in
    let toks =
      to_list [] (lexer (Stream.of_channel fd))
    in
    let nacc =
      analyze fn acc toks
    in
    close_in fd;
    nacc

type action =
  | Depends
  | OCamlify

let () =
  (* Configuration through command line arguments *)
  let all_var = ref [] in
  let action = ref OCamlify in
  let target = ref None in
  let output = ref None in
  let parse_var vartype = 
    Arg.Tuple
      (let varname = ref "" in
       let file = ref "" in
       [ Arg.Set_string varname
       ; Arg.Set_string file
       ; Arg.Unit (fun () -> all_var := (vartype, !varname, !file) :: !all_var)
       ])
  in
  Arg.parse
    (Arg.align
       [
         "--var-string-list",
         parse_var VarStringList,
         "varname file Include file as a list of string, each string \
          representing a line (without trailing EOL character)";

         "--var-string",
         parse_var VarString,
         "varname file Include file as a string.";

         "--output",
         Arg.String (fun str -> output := Some str),
         "file Output file, default to standard output";

         "--depends",
         Arg.Unit (fun () -> action := Depends),
         " Output file dependencies";

         "--target",
         Arg.String (fun str -> target := Some str),
         "file Define target of dependencies";
       ])
    (fun fn -> all_var := parse_file fn !all_var)
    "ocamlify <file>";
  all_var := List.rev !all_var;

  let act fd var_lst =
    match !action with
      | Depends ->
        depends fd var_lst !target
      | OCamlify ->
        ocamlify fd var_lst
  in
  match !output with
    | Some fl ->
      let fd = 
        open_out fl
      in
      (
        try 
          act fd !all_var;
          close_out fd
        with e ->
          (
            close_out fd;
            Sys.remove fl;
            raise e
          )
      )
    | None ->
      act stdout !all_var

