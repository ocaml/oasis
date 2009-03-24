%{

open Oasis;;

%}

%token SEMICOLON
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
  | IDENT SEMICOLON VALUE   {ASTField($1, $3)}
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

