(** OCamlbuild extension, copied from Nicolas Pouillard mail to caml list
  * http://caml.inria.fr/pub/ml-archives/caml-list/2008/03/7e686d65c788f52003e60c9b20d245ff.en.html
  *
  * Modified by Sylvain Le Gall 
  *)

open Ocamlbuild_plugin

(* Win32/Unix env *)
let () = 
  match Sys.os_type with 
    | "Win32" ->
        Options.ext_obj := "obj"
    | _ ->
        ()

(* These functions are not really officially exported *)
let run_and_read = 
  Ocamlbuild_pack.My_unix.run_and_read

let blank_sep_strings = 
  Ocamlbuild_pack.Lexers.blank_sep_strings

(* This lists all supported packages *)
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"

(* This is supposed to list available syntaxes, but I don't know how to do it.
 *)
let find_syntaxes () = 
  ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = 
  S[A"ocamlfind"; x]

let dispatch_ocamlfind = 
  function
    | Before_options ->
        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"
                              
    | After_rules ->
        (* When one link an OCaml library/binary/package, one should use 
         * -linkpkg
         *)
        flag ["ocaml"; "link"] & A"-linkpkg";
              
        (* For each ocamlfind package one inject the -package option when 
         * compiling, computing dependencies, generating documentation and 
         * linking.
         *)
         List.iter 
          begin fun pkg ->
            flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
          end 
          (find_packages ());
        (* Like -package but for extensions syntax. Morover -syntax is useless 
         * when linking. 
         *)
         List.iter 
          begin fun syntax ->
            flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end 
          (find_syntaxes ());

        (* The default "thread" tag is not compatible with ocamlfind. Indeed, the
         * default rules add the "threads.cma" or "threads.cmxa" options when
         * using this tag. When using the "-linkpkg" option with ocamlfind, this
         * module will then be added twice on the command line.  To solve this,
         * one approach is to add the "-thread" option when using the "threads"
         * package using the previous plugin. 
         *)
         flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
         flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);

    | _ -> ()
;;
