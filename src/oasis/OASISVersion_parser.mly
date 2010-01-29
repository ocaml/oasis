%{

open OASISAstTypes

%}

%token <string> VER
%token GT GE EQ LT LE OR AND
%token LPAREN RPAREN
%token EOF
%left OR   /* lowest precedence */
%left AND  /* highest precedence */

%start main 
%type <OASISAstTypes.ver_cmp_t> main
%%
main:
  cmp EOF { $1 }
;

cmp:
| LPAREN cmp RPAREN     { $2 }
| cmp AND cmp           { VCAnd ($1, $3) }
| cmp OR cmp            { VCOr ($1, $3) }
| GT VER                { VCGt $2 }
| GE VER                { VCGe $2 }
| EQ VER                { VCEq $2 }
| LT VER                { VCLt $2 }
| LE VER                { VCLe $2 }
;
