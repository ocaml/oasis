
let w = ['0'-'9' 'a'-'z' 'A'-'Z' '_'] 
let s = ['\t' '\r' '\n' ' ']
let notS = _ # s
let d = ['0'-'9']

let url_scheme = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '.']*
let url_path = ( w + ':'? w* '@')? (notS+) (':' d+)? 
  ('/'|'/'((w|['#' '!' ':' '.' '?' '+' '=' '&' '%' '@' '!' '-' '/'])))?

rule url = parse
  (url_scheme "://" url_path) as lxm  { lxm }

and copyright = parse
  ('(' ['c' 'C'] ')' ' '* (d+)('-' d+)? ','? ' ' _*) as lxm { lxm }

and modul = parse
  (['A'-'Z'] w*) as lxm { lxm }
