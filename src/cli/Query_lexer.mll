{
open Query_parser
}

rule token = parse
  [' ''\t''\n']  { token lexbuf }
| ['A'-'Z' 'a'-'z' '0'-'9' '_']+ as lxm { ID lxm }
| '"'            { STR (str (Buffer.create 13) lexbuf) }
| '('            { LPAREN }
| ')'            { RPAREN }
| '.'            { DOT }
| eof            { EOF }

and str buf = parse
  '\\' '"' {Buffer.add_char buf '"'; str buf lexbuf}
| '"'      {Buffer.contents buf}
| _ as c   {Buffer.add_char buf c; str buf lexbuf}
