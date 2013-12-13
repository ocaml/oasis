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

open Oasis

%}

%token COLON
%token IF ELSE
%token RBRACE LBRACE
%token FLAG
%token LIBRARY
%token EXECUTABLE
%token NOT AND OR RPAREN LPAREN TRUE FALSE
%token EOF
%token <string> IDENT
%token <string> VALUE

%start main
%type <Oasis.stmt> main

%%

main:
  | stmt_list EOF {ASTBlock (List.rev $1)}
;

stmt_list:
  | stmt_list stmt {$2 :: $1}
  |                {[]}
;

stmt:
  | IDENT COLON VALUE   {ASTField($1, $3)}
  | FLAG IDENT stmt         {ASTFlag($2, $3)}
  | LIBRARY IDENT stmt      {ASTLibrary($2, $3)}
  | EXECUTABLE IDENT stmt   {ASTExecutable($2, $3)}
  | IF expr stmt ELSE stmt  {ASTIfThenElse ((fun _ -> true), $3, $5)}
  | IF expr stmt            {ASTIfThenElse ((fun _ -> true), $3, ASTBlock [])}
  | LBRACE stmt_list RBRACE {ASTBlock (List.rev $2)}
;

expr:
  | TRUE                       {ETrue}
  | FALSE                      {EFalse}
  | LPAREN expr RPAREN         {$2}
  | NOT expr                   {ENot $2}
  | expr AND expr              {EAnd ($1, $3)}
  | expr OR expr               {EOr ($1, $3)}
  | FLAG LPAREN IDENT RPAREN   {EFlag ($3)}
  | IDENT LPAREN IDENT RPAREN  {ETest ($1, $3)}
;

%%

