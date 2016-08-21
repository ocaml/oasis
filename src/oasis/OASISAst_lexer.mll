(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2011-2016, Sylvain Le Gall                                    *)
(*  Copyright (C) 2008-2011, OCamlCore SARL                                     *)
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

{
open OASISAst_parser
open OASISUtils
open OASISGettext

let char_of_escape lexbuf =
  function
    | '\\' -> '\\' | '\'' -> '\'' | '"'  -> '"'  | 'n'  -> '\n'
    | 't'  -> '\t' | 'b'  -> '\b' | 'r'  -> '\r' | ' '  -> ' '
    | c -> failwithpf ~lexbuf (f_ "Unknown escape sequence: \\%c") c

let mkbuf () = Buffer.create 64

type t =
  | ASTToken of token
  | Indent of (Lexing.position * Lexing.position * string) list * int
  | Eof

let mkline lexbuf lxm =
  let open Lexing in
  let pos = lexeme_start_p lexbuf in
  pos, {pos with pos_cnum = pos.pos_cnum + (String.length lxm)}, lxm

let token_map = MapString.of_list [
  "if", IF; "else", ELSE;
  (* TODO: only use 'true' and 'false' and drop the case of True and TRUE.
   * In order to do that, estimate the impact on the current oasis files.
   *)
  "true", TRUE; "True", TRUE; "TRUE", TRUE;
  "false", FALSE; "False", FALSE; "FALSE", FALSE;
  "flag", FLAG;
  "Flag", SECTION `Flag;
  "Library", SECTION `Library;
  "Object", SECTION `Object;
  "Executable", SECTION `Executable;
  "SourceRepository", SECTION `SrcRepo;
  "Test", SECTION `Test;
  "Document", SECTION `Doc;
]

type lexer_state = {
  mutable first_call: bool;
  mutable indent: int;
  mutable use_tab: bool option;
  q: token Queue.t;
}

let rec warn_if_mixed_indentation lexst lst =
  match lst, lexst.use_tab with
  | (_, _, line) :: _, None ->
      lexst.use_tab <- Some (String.contains line '\t');
      warn_if_mixed_indentation lexst lst
  | (pos1, pos2, line) :: tl, Some use_tab ->
      let has_tab, has_space =
        String.contains line '\t', String.contains line ' '
      in
      if (has_tab && not use_tab) || (has_space && use_tab) then
        failwithpf ~pos1 ~pos2 (f_ "mixed use of '\\t' and ' ' to indent")
      else
        warn_if_mixed_indentation lexst tl
  | [], _ ->
      ()
}

let eol = '\n' | "\r\n"
let blank = [' ' '\t']
let id = ['A'-'Z''a'-'z''0'-'9''-''_']+
let comment = '#' [^'\n']*

rule token = parse
  | '"'       {ASTToken (qstring (mkbuf ()) lexbuf)}
  | "+:"      {ASTToken (PLUS_COLON(value (mkbuf ()) lexbuf))}
  | ":"       {ASTToken (COLON(value (mkbuf ()) lexbuf))}
  | "$:"      {ASTToken DOLLAR_COLON}
  | '!'       {ASTToken NOT}
  | "&&"      {ASTToken AND}
  | "||"      {ASTToken OR}
  | '('       {ASTToken LPAREN}
  | ')'       {ASTToken RPAREN}
  | comment   {token lexbuf}
  | eof       {Eof}
  | blank     {token lexbuf}
  | id as lxm {
    try
      ASTToken (MapString.find lxm token_map)
    with Not_found ->
      ASTToken (IDENT(lxm))
  }
  | eol {
    let () = Lexing.new_line lexbuf in
    let lst, indent = indentation [] lexbuf in
    Indent (lst, indent)
  }
  | _ as c {failwithpf ~lexbuf "extraneous char %C" c}

and indentation lst = parse
  | (blank* as lxm) (comment? as cmt) eol {
    if lxm <> "" && cmt = "" then begin
      failwithpf ~lexbuf (f_ "extraneous blanks at the beginning of the line")
    end;
    Lexing.new_line lexbuf;
    indentation (mkline lexbuf lxm :: lst) lexbuf
  }
  | blank* as lxm {List.rev (mkline lexbuf lxm :: lst), String.length lxm}

and qstring buf = parse
  | '"' {QSTRING (Buffer.contents buf)}
  | '\\' (_ as esc) {
    Buffer.add_char buf (char_of_escape lexbuf esc);
    qstring buf lexbuf
  }
  | eol {Buffer.add_char buf '\n'; Lexing.new_line lexbuf; qstring buf lexbuf}
  | _ as c {Buffer.add_char buf c; qstring buf lexbuf}

and value buf = parse
  | blank*    {value_no_leading_ws buf lexbuf}

and value_no_leading_ws buf = parse
  | blank* comment? eol {Lexing.new_line lexbuf; Buffer.contents buf}
  | blank* comment? eof {Buffer.contents buf}
  | _ as c {Buffer.add_char buf c; value_no_leading_ws buf lexbuf}

{
let token ~ctxt () =
  (* The state of the lexer. *)
  let lexst =
    {
      first_call = true;
      indent     = 0;
      use_tab    = None;
      q          = Queue.create ();
    }
  in

  let indentation lexbuf =
    let lst, indent = indentation [] lexbuf in
    warn_if_mixed_indentation lexst lst;
    indent
  in

  (* Reflect level of indentation by using { }. *)
  let rec update_indent indent' =
    if indent' < lexst.indent then begin
      Queue.push RBRACE lexst.q;
      lexst.indent <- lexst.indent - 1;
      update_indent indent'
    end else if indent' > lexst.indent then begin
      Queue.push LBRACE lexst.q;
      lexst.indent <- lexst.indent + 1;
      update_indent indent'
    end
  in

  (* Try to extract multiple lines value, if possible. *)
  let multiline str lexbuf =
    let open Buffer in
    let multiline_indent = indentation lexbuf in
    let rec add_continuation first_line indent buf lexbuf =
      if indent >= multiline_indent then begin
        let line = value (mkbuf ()) lexbuf in
        (* Don't add EOL if first line is empty. *)
        if not (first_line && Buffer.length buf = 0) then
          add_char buf '\n';
        (* Remove lines which are only ".", they represents an empty line. *)
        if indent > multiline_indent || line <> "." then begin
          add_string buf (String.make (indent - multiline_indent) ' ');
          add_string buf line
        end;
        add_continuation false (indentation lexbuf) buf lexbuf
      end else begin
        update_indent indent;
        contents buf
      end
    in
    if multiline_indent > lexst.indent then begin
      (* Next line is a continuation, it defines the level of indentation for
       * the rest of the multiline value.
       *)
      let buf = Buffer.create 63 in
      add_string buf str;
      add_continuation true multiline_indent buf lexbuf
    end else begin
      (* Next line is not a continuation. *)
      update_indent multiline_indent;
      str
    end
  in

  let rec next lexbuf =
    if lexst.first_call then begin
      lexst.first_call <- false;
      update_indent (indentation lexbuf);
      next lexbuf
    end else if not (Queue.is_empty lexst.q) then begin
      Queue.pop lexst.q
    end else begin
      match token lexbuf with
      | ASTToken (COLON str) -> COLON (multiline str lexbuf)
      | ASTToken (PLUS_COLON str) -> PLUS_COLON (multiline str lexbuf)
      | ASTToken tok -> tok
      | Eof -> update_indent 0; Queue.push EOF lexst.q; next lexbuf
      | Indent (lst, indent) ->
          warn_if_mixed_indentation lexst lst;
          update_indent indent;
          next lexbuf
    end
  in
  next

}
