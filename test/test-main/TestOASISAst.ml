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


(** Tests for OASISAst_*
    @author Sylvain Le Gall
*)

open OUnit2
open TestCommon


let token_to_string =
  let open OASISAst_parser in
  function
  | EOF -> "EOF"
  | QSTRING s -> Printf.sprintf "%S" s
  | PLUS_COLON s -> Printf.sprintf "+: %S" s
  | DOLLAR_COLON -> "$:"
  | COLON s -> Printf.sprintf ": %S" s
  | IF -> "if"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | ELSE -> "else"
  | SECTION knd -> (OASISSection.string_of_section_kind knd)
  | NOT -> "!"
  | AND -> "&&"
  | OR -> "||"
  | LPAREN -> "("
  | RPAREN -> ")"
  | TRUE -> "true"
  | FALSE -> "false"
  | IDENT s -> Printf.sprintf "IDENT %S" s
  | FLAG -> "flag"


let string_of_top_stmt =
  let open OASISAst_types in
  let ppf fmt = Format.fprintf fmt in
  let rec ppf_top_stmt fmt =
    function
    | TSSection(sct_knd, nm, stmt) ->
      ppf fmt "TSSection(@[<hv>%s,@ %S,@ %a@])"
        (OASISSection.string_of_section_kind sct_knd) nm ppf_stmt stmt
    | TSStmt stmt ->
      ppf fmt "TSStmt(@[<hv>%a@])" ppf_stmt stmt
    | TSBlock (hd :: tl) ->
      ppf fmt "TSBlock(@[<hv>%a" ppf_top_stmt hd;
      List.iter (ppf fmt ",@ %a" ppf_top_stmt) tl;
      ppf fmt "@])"
    | TSBlock [] ->
      ppf fmt "SBlock([])"
  and ppf_stmt fmt =
    function
    | SField(nm, fop) ->
      ppf fmt "SField(@[<hv>%S,@ %a@])" nm ppf_field_op fop
    | SIfThenElse(expr, stmt1, stmt2) ->
      ppf fmt "SIfThenElse(@[<hv>%S,@ %a,@ %a@])"
        (OASISExpr.to_string expr)
        ppf_stmt stmt1 ppf_stmt stmt2
    | SBlock (hd :: tl) ->
      ppf fmt "SBlock(@[<hv>%a" ppf_stmt hd;
      List.iter (ppf fmt ",@ %a" ppf_stmt) tl;
      ppf fmt "@])"
    | SBlock [] ->
      ppf fmt "SBlock([])"
  and ppf_field_op fmt =
    function
    | FSet s -> ppf fmt "FSet(%S)" s
    | FAdd s -> ppf fmt "FAdd(%S)" s
    | FEval expr -> ppf fmt "FEval(%S)" (OASISExpr.to_string expr)
  in
  fun top_stmt ->
    ppf Format.str_formatter "%a@." ppf_top_stmt top_stmt;
    Format.flush_str_formatter ()


let pp_diff_top_stmt =
  let open OASISAst_types in
  let cond_prepend b v l = if b then v :: l else l in
  let rec diff_top_stmt =
    function
    | tstmt1, tstmt2 when tstmt1 = tstmt2 -> []
    | TSSection(sct_knd1, nm1, stmt1), TSSection(sct_knd2, nm2, stmt2) ->
      if sct_knd1 <> sct_knd2 then
        [Printf.sprintf "section kind %s -> %s"
           (OASISSection.string_of_section_kind sct_knd1)
           (OASISSection.string_of_section_kind sct_knd2)]
      else
        cond_prepend (nm1 <> nm2)
          (Printf.sprintf "name %S -> %S" nm1 nm2)
          (diff_stmt (stmt1, stmt2))
    | TSStmt stmt1, TSStmt stmt2 ->
      diff_stmt (stmt1, stmt2)
    | TSBlock lst1, TSBlock lst2 ->
      if List.length lst1 <> List.length lst2 then
        [Printf.sprintf "length %d -> %d" (List.length lst1) (List.length lst2)]
      else
        List.fold_left2
          (fun l tstmt1 tstmt2 -> l @ diff_top_stmt (tstmt1, tstmt2))
          [] lst1 lst2
    | tstmt1, tstmt2 ->
      [Printf.sprintf
         "%s -> %s"
         (string_of_top_stmt tstmt1)
         (string_of_top_stmt tstmt2)]
  and diff_stmt =
    function
    | stmt1, stmt2 when stmt1 = stmt2 -> []
    | SField(nm1, fop1), SField(nm2, fop2) ->
      cond_prepend (nm1 <> nm2)
        (Printf.sprintf "name %S -> %S" nm1 nm2)
        (diff_field_op (fop1, fop2))
    | SIfThenElse(expr1, stmt11, stmt12), SIfThenElse(expr2, stmt21, stmt22) ->
      cond_prepend (expr1 <> expr2)
        (Printf.sprintf
           "eval %s -> %s"
           (OASISExpr.to_string expr1)
           (OASISExpr.to_string expr2))
        (diff_stmt (stmt11, stmt21)) @ (diff_stmt (stmt12, stmt22))
    | SBlock lst1, SBlock lst2 ->
      if List.length lst1 <> List.length lst2 then
        [Printf.sprintf "length %d -> %d" (List.length lst1) (List.length lst2)]
      else
        List.fold_left2 
          (fun l stmt1 stmt2 -> l @ diff_stmt (stmt1, stmt2))
          [] lst1 lst2
    | _, _ -> ["statement change"]
  and diff_field_op =
    function
    | fop1, fop2 when fop1 = fop2 -> []
    | FSet s1, FSet s2
    | FAdd s1, FAdd s2 ->
      [Printf.sprintf "field %S -> %S" s1 s2]
    | FEval expr1, FEval expr2 ->
      [Printf.sprintf
         "eval %s -> %s"
         (OASISExpr.to_string expr1)
         (OASISExpr.to_string expr2)]
    | _, _ -> ["field operation change"]
  in
  fun fmt (tstmt1, tstmt2) ->
    let open Format in
    let _b : bool =
      fprintf fmt "@[<v>";
      List.fold_left
        (fun first str ->
           if not first then fprintf fmt "@,";
           fprintf fmt "%s" str;
           false)
        true
        (diff_top_stmt (tstmt1, tstmt2));
    in
    fprintf fmt "@]"

let tests =
  "OASISAst" >:::
  [
    "OASISAst_lexer" >::
    (fun test_ctxt ->
       let open OASISAst_parser in
       let printer lst = String.concat "; " (List.map token_to_string lst) in
       let rec all lst lexer lexbuf =
         match lexer lexbuf with
         | OASISAst_parser.EOF as tok -> List.rev (tok :: lst)
         | tok -> all (tok :: lst) lexer lexbuf
       in
       List.iter
         (fun (str, exp) ->
            let lexer =
              OASISAst_lexer.token
                ~ctxt:(oasis_ctxt ~ignore_plugin:true test_ctxt)
                ()
            in
            assert_equal ~printer exp (all [] lexer (Lexing.from_string str)))
         [
           (* Empty *)
           "", [EOF];
           (* Simple keyword *)
           "true", [TRUE; EOF];
           (* Empty field *)
           "foobar:", [IDENT "foobar"; COLON ""; EOF];
           (* Empty field + padding *)
           "foobar:  ", [IDENT "foobar"; COLON ""; EOF];
           (* Simple field *)
           "foobar: this is a test",
           [IDENT "foobar"; COLON "this is a test"; EOF];
           (* Multiline field *)
           "foobar: a\n b", [IDENT "foobar"; COLON "a\nb"; EOF];
           (* Multiline field + padding *)
           "foobar: a\n b\n  c", [IDENT "foobar"; COLON "a\nb\n c"; EOF];
           (* Multiline field + empty line *)
           "foobar: a\n .\n c", [IDENT "foobar"; COLON "a\n\nc"; EOF];
           (* Comment *)
           "# comment", [EOF];
           (* Comment + padding *)
           " # comment\nfoobar: a", [IDENT "foobar"; COLON "a"; EOF];
           (* Comment in field *)
           "foobar: a # for b", [IDENT "foobar"; COLON "a"; EOF];
           (* Comment in multiline *)
           "foobar: a \n# for b\n c", [IDENT "foobar"; COLON "a\nc"; EOF];
           (* Comment in multiline *)
           "foobar: a \n b\n# c\n d", [IDENT "foobar"; COLON "a\nb\nd"; EOF];
           (* Complex. *)
           "OASISVersion: >= 3.12.0\n\
            \n\
            Flag abcd\n\
            \ Description: This is a text",
           [IDENT "OASISVersion"; COLON ">= 3.12.0";
            SECTION `Flag; IDENT "abcd";
            LBRACE;
            IDENT "Description"; COLON "This is a text";
            RBRACE; EOF];
         ]);

    "OASISAst" >::
    (fun test_ctxt ->
       let open OASISAst_types in
       let open OASISExpr in
       List.iter
         (fun (str, exp) ->
            let lexbuf = Lexing.from_string str in
            let ctxt = oasis_ctxt test_ctxt in
            let ast =
              OASISAst_parser.main (OASISAst_lexer.token ~ctxt ()) lexbuf
            in
            non_fatal test_ctxt
              (fun _ ->
                 assert_equal
                   ~ctxt:test_ctxt
                   ~printer:string_of_top_stmt
                   ~pp_diff:pp_diff_top_stmt
                   exp ast))
         [
           "FieldName: Value",
           TSStmt(SField("FieldName", FSet("Value")));

           "if ocaml_version(\"4.02\")\n\
           \  FieldName: Value",
           TSStmt(
             SIfThenElse(
               ETest(test_of_string "ocaml_version", "4.02"),
               SField("FieldName", FSet("Value")),
               SBlock []));
         ]);
  ]

