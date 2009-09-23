
(** OCaml Autobuild Source Information System

    @author Sylvain Le Gall
  *)  

(** [from_file fn ?(srcdir) valid_test] Parse the OASIS file [fn]. Consider
    only test defined in [valid_test] when checking OASIS. When testing for
    file/dir existence, consider that root of the project is located in 
    [srcdir].
  *)
let from_file fn ?(srcdir) ?(debug=false) ?(ignore_unknown=false) valid_tests = 
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
    OASISAst.to_package fn ignore_unknown srcdir valid_tests ast
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

(** Add a new field to schema
  *)
let new_field schm plugin nm ?default parse =
  OASISSchema.new_field 
    schm 
    ("x"^plugin^nm) 
    ?default
    ~plugin:plugin
    parse
;;

(** Add a new field to schema which can be conditional
  *)
let new_field_conditional schm plugin nm ?default parse =
  OASISSchema.new_field_conditional 
    schm 
    ("x"^plugin^nm) 
    ?default 
    ~plugin:plugin
    parse 
;;

