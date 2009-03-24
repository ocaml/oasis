{
open OasisParser;;
}

rule token = parse
[' ' '\t']       {token lexbuf}
| '\n'           {Lexing.new_line lexbuf; token lexbuf}
| ":"            {SEMICOLON}
| "if"           {IF}
| "else"         {ELSE}
| "{"            {RBRACE}
| "}"            {LBRACE}
| "Library"      {LIBRARY}
| "Executable"   {EXECUTABLE}
| "Flag"         {FLAG}
| "||"           {OR}
| "&&"           {AND}
| '!'            {NOT}
| '('            {LPAREN}
| ')'            {RPAREN}
| "true"         {TRUE}
| "false"        {FALSE}
| eof            {EOF}
| ['A'-'Z''a'-'z''0'-'9''-''_']+ as lxm {IDENT(lxm)}

