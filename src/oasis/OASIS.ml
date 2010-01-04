
(** OCaml Autobuild Source Information System

    @author Sylvain Le Gall
  *)  

(** [from_file fn ?(srcdir) valid_test] Parse the OASIS file [fn]. Consider
    only test defined in [valid_test] when checking OASIS. When testing for
    file/dir existence, consider that root of the project is located in 
    [srcdir].
  *)
let from_file ?(srcdir) ?(debug=false) ?(ignore_unknown=false) fn = 
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
  let pkg = 
    OASISAst.to_package fn ignore_unknown srcdir ast
  in
    pkg
;;

(** Add a new field to schema
  *)
let new_field = OASISPlugin.new_field
;;

(** Add a new field to schema which can be conditional
  *)
let new_field_conditional = OASISPlugin.new_field_conditional
;;

(** Print help about OASIS fields.
  *)
let pp_help = 
  OASISHelp.pp_help
;;

