
external reident : 'a -> 'a = "caml_reident_custom";;

print_endline (reident (A.ident "hello world!"));;
