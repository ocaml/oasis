(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
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


(** Parse OASIS files using Genlex.
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISAstTypes
open OASISGettext
open OASISUtils
open OASISExpr
open OASISContext
open OASISMessage
open Genlex


(** Configuration for parsing and checking
*)
type conf =
  {
    oasisfn: host_filename option;
    ctxt:    OASISContext.t;
  }


type line_pos = int
type char_pos = int


type t =
  | RealLine of line_pos * char_pos * string (* line, begin char, data *)
  | BlockBegin
  | BlockEnd
  | StringBegin
  | StringEnd


let parse_stream conf st =

  let is_blank =
    function
      | ' ' | '\r' | '\t' -> true
      | _ -> false
  in

  let position lineno charno =
    Printf.sprintf
      (f_ "in file '%s' at line %d, char %d")
      (match conf.oasisfn with
        | Some fn -> fn
        | None -> "<>")
      lineno
      charno
  in

  (* Override OASISMessage functions *)
  let debug fmt =
    debug ~ctxt:conf.ctxt fmt
  in
  let warning fmt =
    warning ~ctxt:conf.ctxt fmt
  in

  let apply_transformations ops =
    List.fold_left
      (fun lines (msg, f) ->
         let new_lines =
           f lines
         in
         if conf.ctxt.debug && lines <> new_lines then
           begin
             let rec pp_lines mode =
               function
                 | RealLine (lineno, charstart, str) :: tl ->
                   debug "%s%04d:%s->%s<-"
                     mode
                     lineno
                     (String.make charstart ' ')
                     str;
                   pp_lines mode tl

                 | (BlockBegin as e) :: tl
                 | (BlockEnd as e) :: tl ->
                   pp_blocks mode 1 e tl
                 | StringBegin :: tl ->
                   pp_lines "s" tl
                 | StringEnd :: tl ->
                   pp_lines " " tl
                 | [] ->
                   ()

             and pp_blocks mode lvl e =
               function
                 | b :: tl when b = e ->
                   pp_blocks mode (lvl + 1) e tl
                 | lst ->
                   debug "%s----:%s"
                     mode
                     (String.make
                        lvl
                        (match e with
                          | BlockBegin -> '{'
                          | BlockEnd   -> '}'
                          | RealLine _ | StringBegin | StringEnd ->
                            assert(false)));
                   pp_lines mode lst
             in

             debug "%s:" msg;
             pp_lines " " new_lines;
             debug (f_ "EOF")
           end
         else if conf.ctxt.debug then
           begin
             debug (f_ "Nothing changed for '%s'") msg
           end;
         new_lines)
      []
      ops
  in

  let lines =
    apply_transformations
      [
        "Extract all lines from the stream",
        (fun _ ->
           let lines =
             ref []
           in
           let buff =
             Buffer.create 13
           in
           let lineno =
             ref 1
           in
           let add_line () =
             lines := (RealLine (!lineno, 0, Buffer.contents buff)) :: !lines;
             Buffer.clear buff;
             incr lineno
           in
           Stream.iter
             (function
               | '\n' ->
                 add_line ()
               | c ->
                 Buffer.add_char buff c)
             st;
           add_line ();
           List.rev !lines);

        "Get rid of MS-DOS file format",
        List.map
          (function
            | RealLine (lineno, charstart, str) ->
              begin
                let len =
                  String.length str
                in
                let str =
                  if len > 0 && str.[len - 1] = '\r' then
                    String.sub str 0 (len - 1)
                  else
                    str
                in
                RealLine (lineno, charstart, str)
              end
            | e -> e);

        "Remove comments",
        List.map
          (function
            | RealLine (lineno, charstart, str) ->
              begin
                try
                  let idx =
                    String.index str '#'
                  in
                  RealLine (lineno, charstart, String.sub str 0 idx)
                with Not_found ->
                  RealLine(lineno, charstart, str)
              end
            | e -> e);

        "Remove trailing whitespaces",
        List.map
          (function
            | RealLine (lineno, charstart, str) ->
              begin
                let pos =
                  ref ((String.length str) - 1)
                in
                while !pos > 0 && is_blank str.[!pos] do
                  decr pos
                done;
                RealLine(lineno, charstart, String.sub str 0 (!pos + 1))
              end
            | e -> e);

        "Remove empty lines",
        List.filter
          (function
            | RealLine (lineno, charstart, "") -> false
            | _ -> true);

        "Remove leading whitespaces and replace them with begin/end block",
        (fun lines ->
           let add_blocks n lst =
             List.rev_append
               (Array.to_list
                  (Array.init
                     (abs n)
                     (if n < 0 then
                        (fun _ -> BlockEnd)
                      else
                        (fun _ -> BlockBegin))))
               lst
           in
           let lines, last_indent_level, _ =
             List.fold_left
               (fun (lines, prv_indent_level, only_tab) ->
                  function
                    | RealLine (lineno, charstart, str) ->
                      begin
                        let cur_indent_level, use_tab, use_space =
                          let pos =
                            ref 0
                          in
                          let use_tab =
                            ref false
                          in
                          let use_space =
                            ref false
                          in
                          while !pos < String.length str &&
                                is_blank str.[!pos] do
                            use_tab   := str.[!pos] = '\t' || !use_tab;
                            use_space := (not (str.[!pos] = '\t')
                                          || !use_space);
                            incr pos
                          done;
                          !pos, !use_tab, !use_space
                        in
                        let only_tab =
                          if use_space && use_tab then
                            begin
                              warning
                                (f_ "Mixed use of '\\t' and ' ' to indent \
                                     lines %s")
                                (position lineno charstart);
                              only_tab
                            end
                          else
                            begin
                              match only_tab with
                                | Some use_tab_before ->
                                  if use_tab_before && not use_tab then
                                    warning
                                      (f_ "Use of ' ' but '\\t' was used \
                                           before to indent lines %s")
                                      (position lineno charstart);
                                  if not use_tab_before && use_tab then
                                    warning
                                      (f_ "Use of '\\t' but ' ' was used \
                                           before to indent lines %s")
                                      (position lineno charstart);

                                  only_tab
                                | None ->
                                  Some use_tab
                            end
                        in
                        let charstart =
                          charstart + cur_indent_level
                        in
                        let str =
                          String.sub
                            str
                            cur_indent_level
                            ((String.length str) - cur_indent_level)
                        in
                        let diff_indent_level =
                          cur_indent_level - prv_indent_level
                        in
                        let lines =
                          RealLine (lineno, charstart, str)
                          ::
                            add_blocks diff_indent_level lines
                        in
                        lines, cur_indent_level, only_tab
                      end
                    | BlockBegin as e ->
                      (e :: lines), prv_indent_level + 1, only_tab
                    | BlockEnd as e ->
                      (e :: lines), prv_indent_level - 1, only_tab
                    | StringBegin | StringEnd as e ->
                      (e :: lines), prv_indent_level, only_tab)
               ([], 0, None)
               lines
           in
           List.rev
             (add_blocks
                (* Return back to indent level 0 *)
                (~- last_indent_level)
                lines));

        "Split values and detect multi-line values",
        (fun lines ->
           let rec find_field acc =
             function
               | RealLine (lineno, charstart, str) as e :: tl ->
                 begin
                   try
                     let colon_pos =
                       String.index str ':'
                     in
                     let pos =
                       ref (colon_pos + 1)
                     in
                     let () =
                       while !pos < String.length str && is_blank str.[!pos] do
                         incr pos
                       done
                     in
                     let line_begin =
                       RealLine (lineno, charstart,
                         String.sub str 0 (colon_pos + 1))
                     in
                     let acc =
                       if !pos < String.length str then
                         begin
                           (* Split end of line *)
                           let line_end =
                             RealLine
                               (lineno,
                                charstart + !pos,
                                String.sub str !pos (String.length str - !pos))
                           in
                           line_end :: StringBegin :: line_begin :: acc
                         end
                       else
                         begin
                           StringBegin :: line_begin :: acc
                         end
                     in
                     fetch_multiline acc 0 tl

                   with Not_found ->
                     find_field (e :: acc) tl
                 end

               | e :: tl ->
                 find_field (e :: acc) tl

               | [] ->
                 List.rev acc

           and fetch_multiline acc lvl lst =

             let lvl_ref =
               let rec count_block_begin lvl =
                 function
                   | BlockBegin :: tl ->
                     (* Count the initial indentation (first line) *)
                     count_block_begin (lvl + 1) tl
                   | lst ->
                     lvl
               in
               count_block_begin lvl lst
             in

             let rec fetch_multiline_nxt acc lvl =
               function
                 | BlockBegin :: tl ->
                   fetch_multiline_nxt acc (lvl + 1) tl

                 | BlockEnd :: tl ->
                   if lvl > 1 then
                     fetch_multiline_nxt acc (lvl - 1) tl
                   else if lvl = 1 then
                     find_field (StringEnd :: acc) tl
                   else
                     find_field (StringEnd :: acc) (BlockEnd :: tl)

                 | RealLine (lineno, charstart, str) as e :: tl ->
                   if lvl > 0 then
                     let diff =
                       lvl - lvl_ref
                     in
                     if diff < 0 then
                       failwithf
                         (f_ "Unexpected indentation line %d.")
                         lineno;
                     fetch_multiline_nxt
                       (RealLine
                          (lineno,
                           charstart - diff,
                           String.make diff ' ' ^ str) :: acc)
                       lvl
                       tl
                   else
                     find_field (StringEnd :: acc) (e :: tl)

                 | (StringBegin as e) :: tl
                 | (StringEnd as e) :: tl ->
                   if lvl > 0 then
                     fetch_multiline_nxt acc lvl tl
                   else
                     find_field (StringEnd :: acc) (e :: tl)

                 | [] ->
                   find_field (StringEnd :: acc) []
             in
             fetch_multiline_nxt acc lvl lst
           in

           find_field [] lines);


        "Replace line containing '.' only by blank line",
        (let rec replace_dot =
           function
             | (RealLine (l1, _, _) as e1) ::
                 RealLine (l2, c, ".") ::
                 (RealLine (l3, _, _) as e2) ::
                 tl when l1 <> l2 && l2 <> l3 ->
               e1 :: RealLine (l2, c, "") :: replace_dot (e2 :: tl)

             | (RealLine (l1, _, _) as e1) ::
                 RealLine (l2, c, ".") ::
                 StringEnd :: tl when l1 <> l2 ->
               e1 :: RealLine (l2, c, "") :: StringEnd :: lookup_string tl

             | e :: tl ->
               e :: replace_dot tl

             | [] ->
               []

         and lookup_string =
           function
             | StringBegin :: tl ->
               StringBegin :: replace_dot tl
             | e :: tl ->
               e :: lookup_string tl
             | [] ->
               []
         in
         lookup_string);

        "Add EOL",
        (let rec concat_string =
           function
             | RealLine (l1, charstart, str) ::
                 ((RealLine (l2, _, _) :: _) as tl) when l2 <> l1 ->
               RealLine (l1, charstart, str^"\n") :: concat_string tl
             | StringEnd :: tl ->
               StringEnd :: lookup_string tl
             | hd :: tl ->
               hd :: concat_string tl
             | [] ->
               []
         and lookup_string =
           function
             | StringBegin :: tl ->
               StringBegin :: concat_string tl
             | e :: tl ->
               e :: lookup_string tl
             | [] ->
               []
         in
         lookup_string)
      ]
  in

  let position, st =
    let lineno =
      ref 0
    in
    let charno =
      ref 0
    in
    let virtual_pos =
      ref None
    in

    let lines_q =
      let q =
        Queue.create ()
      in
      List.iter
        (fun s -> Queue.add s q)
        lines;
      q
    in

    let chars_q =
      Queue.create ()
    in

    let in_string =
      ref false
    in

    let rec getc () =
      try
        incr charno;
        Some (Queue.take chars_q)
      with Queue.Empty ->
        begin
          (* Refill the char queue *)
          try
            let () =
              match Queue.take lines_q with
                | RealLine (nlineno, ncharno, str) ->
                  lineno := nlineno;
                  charno := ncharno;
                  String.iter
                    (fun c -> Queue.add c chars_q)
                    (if !in_string then
                       String.escaped str
                     else
                       str);
                | BlockBegin ->
                  virtual_pos := Some "block begin";
                  Queue.add '{'chars_q
                | BlockEnd ->
                  virtual_pos := Some "block end";
                  Queue.add '}' chars_q
                | StringBegin | StringEnd ->
                  virtual_pos := Some "string marker";
                  in_string := not !in_string;
                  Queue.add '"' chars_q
            in
            getc ()

          with Queue.Empty ->
            (* lines and char queue are empty -> nothing left *)
            None
        end
    in

    (fun fmt -> position !lineno !charno),
    Stream.from (fun _ -> getc ())
  in

  (* Lexer for OASIS language *)
  let lexer =
    make_lexer
      [
        (* Statement *)
        "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
        "Flag"; "Library"; "Object"; "Executable";
        "SourceRepository"; "Test";
        "Document";
        (* Expression *)
        "!"; "&&"; "||"; "("; ")";
        (* Boolean *)
        "true"; "True"; "TRUE"; "false"; "False"; "FALSE"
      ]
  in

  (* OASIS expression *)
  let rec parse_factor =
    parser
  | [< 'Kwd "true" >] ->
    EBool true
  | [< 'Kwd "True" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "TRUE" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "false" >] ->
    EBool false
  | [< 'Kwd "False" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "FALSE" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "!"; e = parse_factor >] ->
    ENot e
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] ->
    e
  | [< 'Ident nm; 'Kwd "("; 'Ident vl; 'Kwd ")" >] ->
    if nm = "flag" then
      EFlag vl
    else
      ETest (OASISExpr.test_of_string nm, vl)
  and parse_term_follow =
    parser
  | [< 'Kwd "&&"; e1 = parse_factor; e2 = parse_term_follow >] ->
    EAnd (e1, e2)
  | [< >] ->
    EBool true
  and parse_term =
    parser
  | [< e1 = parse_factor; e2 = parse_term_follow >] ->
    EAnd(e1, e2)

  and parse_expr_follow =
    parser
  | [< 'Kwd "||"; e1 = parse_term; e2 = parse_expr_follow >] ->
    EOr (e1, e2)
  | [< >] ->
    EBool false
  and parse_expr =
    parser
  | [< e1 = parse_term; e2 = parse_expr_follow >] ->
    EOr (e1, e2)
  in

  (* OASIS fields and flags *)
  let rec parse_else =
    parser
  | [< 'Kwd "else"; else_blk = parse_stmt>] ->
    else_blk
  | [< >] ->
    SBlock []

  and parse_field_op =
    parser
  | [<'Kwd ":"; 'String str>] ->
    FSet str

  | [<'Kwd "+:"; 'String str>] ->
    FAdd str

  | [<'Kwd "$:"; 'String str>] ->
    let e =
      try
        parse_expr (lexer (Stream.of_string str))
      with e ->
        failwithf
          (f_ "Error when parsing expression '%s' %t: %s")
          str
          position
          (Printexc.to_string e)
    in
    FEval e

  and parse_stmt =
    parser
  | [<'Ident nm; op = parse_field_op>] ->
    SField(nm, op)

  | [< 'Kwd "if"; e = parse_expr;
       if_blk = parse_stmt;
       else_blk = parse_else>] ->
    SIfThenElse(e, if_blk, else_blk)

  | [< 'Kwd "{"; lst = parse_stmt_list; 'Kwd "}">] ->
    SBlock lst

  and parse_stmt_list =
    parser
  | [< stmt = parse_stmt; tl = parse_stmt_list>] ->
    stmt :: tl
  | [< >] ->
    []
  in

  let id_or_string =
    parser
  | [<'Ident nm>] ->
    nm
  | [<'String nm>] ->
    nm
  in

  let rec parse_top_stmt =
    parser
  | [<'Kwd "Flag"; nm = id_or_string; flag_blk = parse_stmt>] ->
    TSFlag(nm, flag_blk)

  | [<'Kwd "Library"; nm = id_or_string; library_blk = parse_stmt>] ->
    TSLibrary (nm, library_blk)

  | [<'Kwd "Object"; nm = id_or_string; object_blk = parse_stmt>] ->
    TSObject (nm, object_blk)

  | [< 'Kwd "Executable"; nm = id_or_string; exec_blk = parse_stmt>] ->
    TSExecutable (nm, exec_blk)

  | [< 'Kwd "SourceRepository"; nm = id_or_string;
       src_repo_blk = parse_stmt>] ->
    TSSourceRepository (nm, src_repo_blk)

  | [< 'Kwd "Test"; nm = id_or_string; test_blk = parse_stmt>] ->
    TSTest (nm, test_blk)

  | [< 'Kwd "Document"; nm = id_or_string; test_blk = parse_stmt>] ->
    TSDocument (nm, test_blk)

  | [< 'Kwd "{"; lst = parse_top_stmt_list; 'Kwd "}">] ->
    TSBlock lst

  | [< stmt = parse_stmt >] ->
    TSStmt stmt

  and parse_top_stmt_list =
    parser
  | [< top_stmt = parse_top_stmt; tl = parse_top_stmt_list>] ->
    top_stmt :: tl
  | [< >] ->
    []
  in
  (* Main loop *)
  try
    let st_token =
      lexer st
    in
    let ast =
      TSBlock (parse_top_stmt_list st_token)
    in
    ast
  with
    | Stream.Error "" ->
      failwithf (f_ "Syntax error %t") position
    | Stream.Error str ->
      failwithf (f_ "Syntax error %s %t") str position
