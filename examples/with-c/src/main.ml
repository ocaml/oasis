
external reident : 'a -> 'a = "caml_reident";;

print_endline (reident (A.ident "hello world!"));;
