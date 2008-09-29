
open Ocamlbuild_plugin

let packages =
  [
    "findlib";
    "fileutils"
  ]

let ocamlfind x = S[A"ocamlfind"; A x]

let () =
dispatch begin function
  | Before_options ->
      (* Options.include_dirs := "src" :: !Options.include_dirs; *)
      Options.make_links := false;
      (* Override default commands by ocamlfind ones *)
      Options.ocamlc := ocamlfind "ocamlc";
      Options.ocamlopt := ocamlfind "ocamlopt";
      Options.ocamldep := ocamlfind "ocamldep";
      (* There seems to be no other way to specify ocamldoc... *)
      Options.ocamldoc := ocamlfind "ocamldoc";

  | After_rules ->
      (* For each ocamlfind package one inject the -package option
       when compiling, computing dependencies, generating
       documentation and linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link"; "pkg_"^pkg] & S[A"-package"; A pkg];
      end packages;

      (* The default "thread" tag is not compatible with ocamlfind.
       Indeed, the default rules add the "threads.cma" or
       "threads.cmxa" options when using this tag. When using the
       "-linkpkg" option with ocamlfind, this module will then be
       added twice on the command line.

       To solve this, one approach is to add the "-thread" option
       when using the "threads" package using the previous
       plugin. *)
      flag ["ocaml"; "pkg_threads"; "compile"] & S[A "-thread"];
      flag ["ocaml"; "pkg_threads"; "link"] & S[A "-thread"];

  | _ -> ()
end


