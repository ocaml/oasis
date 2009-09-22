
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
    OASISAst.to_package fn srcdir valid_tests ast
;;


(** Print help about OASIS fields.
  *)
let pp_help fmt () = 
  List.iter 
    (OASISSchema.pp_help fmt) 
    [
      OASISPackage.schema;
      OASISFlag.schema;
      OASISLibrary.schema;
      OASISExecutable.schema;
    ]
;;
