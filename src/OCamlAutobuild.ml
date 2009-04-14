
(** Main for OCaml-autobuild *)

open BaseGenNone;;
open BaseGenInternal;;
open OCamlbuildGen;;
open BaseGenerate;;

let () =

  let oasis_fn =
    ref "_oasis"
  in

  let () = 
    Arg.parse 
      (Arg.align 
         [
           "-C",
           (Arg.String (fun str -> Sys.chdir str)),
           "dir Change directory before running.";
         ])
      (fun str -> failwith ("Don't know what to do with '"^str^"'"))
      "ocaml-autobuild (C) 2009 Sylvain Le Gall\n\
       \n\
       ocaml-autobuild [options*] -action [action-options*]\n\n";
  in

  let ast = 
    OasisTools.parse_file !oasis_fn
  in

  let ctxt =
    Oasis.check [] !oasis_fn ast
  in

  let pkg =
    Oasis.oasis (ast, ctxt)
  in

    generate
      {
        ast     = ast;
        pre_pkg = pkg;
      }
;;
