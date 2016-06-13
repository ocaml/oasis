/********************************************************************************/
/*  OASIS: architecture for building OCaml libraries and applications           */
/*                                                                              */
/*  Copyright (C) 2011-2013, Sylvain Le Gall                                    */
/*  Copyright (C) 2008-2011, OCamlCore SARL                                     */
/*                                                                              */
/*  This library is free software; you can redistribute it and/or modify it     */
/*  under the terms of the GNU Lesser General Public License as published by    */
/*  the Free Software Foundation; either version 2.1 of the License, or (at     */
/*  your option) any later version, with the OCaml static compilation           */
/*  exception.                                                                  */
/*                                                                              */
/*  This library is distributed in the hope that it will be useful, but         */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  */
/*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          */
/*  details.                                                                    */
/*                                                                              */
/*  You should have received a copy of the GNU Lesser General Public License    */
/*  along with this library; if not, write to the Free Software Foundation,     */
/*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               */
/********************************************************************************/

%{

open OASISAst_types
open OASISExpr
open OASISUtils
open OASISGettext

let parse_error s =
  failwithpf
    ~pos1:(Parsing.symbol_start_pos ())
    ~pos2:(Parsing.symbol_end_pos ())
    "%s" (s_ "syntax error")

%}

%token <string> PLUS_COLON
%token <string> COLON
%token DOLLAR_COLON
%token IF ELSE
%token RBRACE LBRACE
%token <OASISTypes.section_kind> SECTION
%token NOT AND OR LPAREN RPAREN TRUE FALSE
%token EOF
%token <string> IDENT
%token <string> QSTRING

%left OR      /* Lowest precedence */
%left AND     /* Medium precedence */
%nonassoc NOT /* Highest precedence */

%start main
%type <OASISAst_types.top_stmt> main

%%

main:
  | top_stmt_list EOF {norm (TSBlock (List.rev $1))}
;

top_stmt_list:
  | top_stmt_list top_stmt {$2 :: $1}
  |                        {[]}
;

top_stmt:
  | SECTION id_or_qstring sblock {TSSection($1, $2, $3)}
  | LBRACE top_stmt_list  RBRACE {TSBlock (List.rev $2)}
  | stmt                         {TSStmt($1)}
;

stmt_list:
  | stmt_list stmt {$2 :: $1}
  |                {[]}
;

stmt:
  | IDENT COLON                {SField($1, FSet($2))}
  | IDENT PLUS_COLON           {SField($1, FAdd($2))}
  | IDENT DOLLAR_COLON expr    {SField($1, FEval($3))}
  | IF expr sblock elsesblock  {SIfThenElse($2, $3, $4)}
  | sblock                     {$1}
;

elsesblock:
  | ELSE IF expr sblock elsesblock {SIfThenElse($3, $4, $5)}
  | ELSE sblock                    {$2}
  |                                {SBlock []}
;

sblock:
  | LBRACE stmt_list RBRACE {SBlock (List.rev $2)}
;

expr:
  | TRUE                      {EBool true}
  | FALSE                     {EBool false}
  | LPAREN expr RPAREN        {$2}
  | NOT expr                  {ENot $2}
  | expr AND expr             {EAnd ($1, $3)}
  | expr OR expr              {EOr ($1, $3)}
  | IDENT LPAREN IDENT RPAREN {
    if OASISString.lowercase_ascii $1 = "flag" then
      EFlag($3)
    else
      ETest (test_of_string $1, $3)
  }
;

id_or_qstring:
  | IDENT   {$1}
  | QSTRING {$1}
;
%%
