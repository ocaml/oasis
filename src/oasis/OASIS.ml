
(** OCaml Autobuild Source Information System

    @author Sylvain Le Gall
  *)  

(** [from_file fn ?(srcdir) valid_test] Parse the OASIS file [fn]. Consider
    only test defined in [valid_test] when checking OASIS. When testing for
    file/dir existence, consider that root of the project is located in 
    [srcdir].
  *)
let from_file fn ?(srcdir) ?(debug=false) valid_tests = 
  let srcdir =
    match srcdir with 
      | Some fn ->
          fn
      | None ->
          Filename.dirname fn
  in

  let ast = 
    OASISRecDescParser.parse_file ~debug fn
  in
  let ctxt = 
    OASISAst.check valid_tests fn srcdir ast
  in
    OASISAst.oasis (ast, ctxt)
;;

