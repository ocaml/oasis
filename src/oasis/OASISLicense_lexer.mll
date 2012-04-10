
{
open OASISLicense_parser
}

rule token = parse 
  [' ''\t''\n'] { token lexbuf }
| ','           { COMMA }
| "and"         { AND }
| "or"          { OR }
| "with"        { WITH }
| "exception"   { EXCEPTION }
| eof           { EOF }
| [^' ''\t''\n'',']+ as lxm { TOKEN lxm }
