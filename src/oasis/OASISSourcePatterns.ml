(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

open OASISUtils
open OASISGettext

module Templater =
struct
  (* TODO: use this module in BaseEnv.var_expand and BaseFileAB, at least. *)
  type t =
    {
      atoms: atom list;
      origin: string
    }
  and atom =
    | Text of string
    | Expr of expr
  and expr =
    | Ident of string
    | String of string
    | Call of string * expr


  type env =
    {
      variables: string MapString.t;
      functions: (string -> string) MapString.t;
    }


  let eval env t =
    let rec eval_expr env =
      function
      | String str -> str
      | Ident nm ->
        begin
          try
            MapString.find nm env.variables
          with Not_found ->
            (* TODO: add error location within the string. *)
            failwithf
              (f_ "Unable to find variable %S in source pattern %S")
              nm t.origin
        end

      | Call (fn, expr) ->
        begin
          try
            (MapString.find fn env.functions) (eval_expr env expr)
          with Not_found ->
            (* TODO: add error location within the string. *)
            failwithf
              (f_ "Unable to find function %S in source pattern %S")
              fn t.origin
        end
    in
    String.concat ""
      (List.map
         (function
           | Text str -> str
           | Expr expr -> eval_expr env expr)
         t.atoms)


  let parse env s =
    let lxr = Genlex.make_lexer [] in
    let parse_expr s =
      let st = lxr (Stream.of_string s) in
      match Stream.npeek 3 st with
      | [Genlex.Ident fn; Genlex.Ident nm] -> Call(fn, Ident nm)
      | [Genlex.Ident fn; Genlex.String str] -> Call(fn, String str)
      | [Genlex.String str] -> String str
      | [Genlex.Ident nm] -> Ident nm
      (* TODO: add error location within the string. *)
      | _ -> failwithf (f_ "Unable to parse expression %S") s
    in
    let parse s =
      let lst_exprs = ref [] in
      let ss =
        let buff = Buffer.create (String.length s) in
        Buffer.add_substitute
          buff
          (fun s -> lst_exprs := (parse_expr s) :: !lst_exprs; "\000")
          s;
        Buffer.contents buff
      in
      let rec join =
        function
        | hd1 :: tl1, hd2 :: tl2 -> Text hd1 :: Expr hd2 :: join (tl1, tl2)
        | [], tl -> List.map (fun e -> Expr e) tl
        | tl, [] -> List.map (fun e -> Text e) tl
      in
      join (OASISString.nsplit ss '\000', List.rev (!lst_exprs))
    in
    let t = {atoms = parse s; origin = s} in
    (* We rely on a simple evaluation for checking variables/functions.
       It works because there is no if/loop statement.
    *)
    let _s : string = eval env t in
    t

(* END EXPORT *)
  let odn_of_t t =
    let open OASISDataNotation in
    let modul = "OASISSourcePatterns.Templater" in
    let rec of_atom a =
      let nm, args =
        match a with
        | Text str -> "Text", [STR str]
        | Expr expr -> "Expr", [of_expr expr]
      in
      VRT(modul ^ "." ^ nm, args)
    and of_expr expr =
      let nm, args =
        match expr with
        | Ident str -> "Ident", [STR str]
        | String str -> "String", [STR str]
        | Call(str, expr) -> "Call", [STR str; of_expr expr]
      in
      VRT(modul ^ "." ^ nm, args)
    in
    REC(
      modul, ["atoms", LST (List.map of_atom t.atoms); "origin", STR t.origin])
(* START EXPORT *)
end


type t = Templater.t


let env ~modul () =
  {
    Templater.
    variables = MapString.of_list ["module", modul];
    functions = MapString.of_list
        [
          "capitalize_file", OASISUnixPath.capitalize_file;
          "uncapitalize_file", OASISUnixPath.uncapitalize_file;
        ];
  }

let all_possible_files lst ~path ~modul =
  let eval = Templater.eval (env ~modul ()) in
  List.fold_left
    (fun acc pat -> OASISUnixPath.concat path (eval pat) :: acc)
    [] lst


let to_string t = t.Templater.origin


(* END EXPORT *)


let parse = Templater.parse (env ~modul:"Foo" ())


let interface =
  List.map parse
    [
      "${capitalize_file module}.mli";
      "${uncapitalize_file module}.mli";
    ]


let implementation =
  List.map parse
    [
      "${capitalize_file module}.ml";
      "${uncapitalize_file module}.ml";
      "${capitalize_file module}.mll";
      "${uncapitalize_file module}.mll";
      "${capitalize_file module}.mly";
      "${uncapitalize_file module}.mly";
    ]


let odn_of_t = Templater.odn_of_t

