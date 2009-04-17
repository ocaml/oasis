
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

  let pkg =
    OASIS.from_file !oasis_fn BaseOCamlcConfig.config
  in

    generate pkg
;;
