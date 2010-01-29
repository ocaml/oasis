{
open OASISVersion_parser
}

rule token = parse
  [' ''\t''\n']  { token lexbuf }
| '>'            { GT }
| ">="           { GE }
| '='            { EQ }
| '<'            { LT }
| "<="           { LE }
| "&&"           { OR }
| "||"           { AND }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { EOF }
| [^' ''\t''\n']+ as lxm { VER lxm }

