%{
  open Query_types
%}

%token <string> ID
%token <string> STR
%token LPAREN RPAREN
%token DOT
%token EOF

%start main
%type <Query_types.t> main
%%
main:
|  ID LPAREN value RPAREN DOT ID EOF { QuerySectionField($1, $3, $6) }
|  ID EOF {
  match $1 with
  | "ListSections" -> QueryListSections
  | "ListFields" -> QueryListFields
  | s -> QueryField(s)
}
;

value:
| ID  { $1 }
| STR { $1 }
;
