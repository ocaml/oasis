
external reident : 'a -> 'a = "caml_reident_native";;

print_endline (reident (A.ident "hello world!"));;
