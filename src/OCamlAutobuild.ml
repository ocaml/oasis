
(** Main for OCaml-autobuild *)

open NoneGen;;
open InternalGen;;
open OCamlbuildGen;;
open CustomGen;;
open METAGen;;
open DevFilesGen;;
open StdFilesGen;;

open BaseGenerate;;
open CommonGettext;;

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
           (s_ "dir Change directory before running.");

           "--oasis-help",
           (Arg.Unit 
              (fun () ->
                 OASIS.pp_help Format.std_formatter ();
                 exit 0)),
           (s_ " Print help about OASIS schema.");

         ])
      (fun str -> 
         failwith 
           (Printf.sprintf 
              (f_ "Don't know what to do with '%s'")
              str))
      (s_ "ocaml-autobuild (C) 2009 Sylvain Le Gall\n\
           \n\
           ocaml-autobuild [options*] -action [action-options*]\n\n");
  in

  let pkg =
    OASIS.from_file !oasis_fn BaseOCamlcConfig.config
  in

    generate pkg;
;;
