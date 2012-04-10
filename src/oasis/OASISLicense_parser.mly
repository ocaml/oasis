
%{
open OASISLicense_types
%}

%token <string> TOKEN
%token COMMA
%token AND
%token OR 
%token OR
%token WITH
%token EXCEPTION
%token EOF
%left COMMA
%left OR
%left AND

%start main
%type <OASISLicense_types.t> main
%%

main:
  licenses EOF { $1 }
;

licenses:
| licenses AND licenses   { And ($1, $3) }
| licenses OR  licenses   { Or  ($1, $3) }
| licenses COMMA AND licenses { And ($1, $4) }
| licenses COMMA OR licenses  { Or($1, $4) }
| TOKEN WITH tokens EXCEPTION { let excpt = String.concat " " (List.rev $3) in License($1, Some excpt) }
| TOKEN { License($1, None) }
;

tokens:
| tokens TOKEN { $2 :: $1 }
| TOKEN        { [$1] }
;
