
open Ocamlbuild_plugin;;
open Command;;

let depends_from_file env build ?(fmod=fun x -> x) fn =
  let depends_lst = 
    let deps = 
      ref []
    in
    let fd = 
      open_in  fn
    in
      (
        try
          while true; do
            deps := (fmod (input_line fd)) :: !deps
          done;
        with End_of_file ->
          ()
      );
      close_in fd;
      List.rev !deps
  in
    List.iter 
      (fun fn ->
         List.iter
           (function
              | Outcome.Good _ -> ()
              | Outcome.Bad exn -> 
                  prerr_endline 
                    (Printf.sprintf 
                       "Could not build '%s': %s"
                       fn
                       (Printexc.to_string exn));

                  raise exn
           ) 
           (build [[fn]])
      )
      depends_lst
;;

let ocamlmod_str = "src/tools/ocamlmod.byte";;
let ocamlmod = A ocamlmod_str;;

rule "ocamlmod: %.mod -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mod"; ocamlmod_str]
  begin
    fun env build ->
      let modfn =
        env "%.mod"
      in
      let dirname =
        Pathname.dirname modfn
      in
        depends_from_file 
          env 
          build
          ~fmod:(fun fn -> dirname/fn)
          modfn;
        Cmd(S[ocamlmod;
              P(modfn)])
  end
;;

let ocamlify = A"ocamlify";;

rule "ocamlify: %.mlify -> %.mlify.depends"
  ~prod:"%.mlify.depends"
  ~dep:"%.mlify"
  begin
    fun env _ -> 
      Cmd(S[ocamlify; 
            T(tags_of_pathname (env "%.mlify")++"ocamlify"++"depends");
            A"--depends"; 
            A"--output"; P(env "%.mlify.depends"); 
            P(env "%.mlify");])
  end
;;

rule "ocamlify: %.mlify & %.mlify.depends -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mlify"; "%.mlify.depends"]
  begin 
    fun env build ->
      depends_from_file 
        env 
        build
        (env "%.mlify.depends");
      Cmd(S[ocamlify; A"--output"; P(env "%.ml"); P(env "%.mlify")])
  end
;;

(** COPIED FROM src/ocamlbuild/myocamlbuild *)
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
         flag ["ocaml"; "pkg_threads"; "compile"] & (S[A"-thread"]);
         flag ["ocaml"; "pkg_threads"; "link"]    & (S[A"-thread"]);

         flag ["ocaml"; "pp_camlp4o"; "compile"]  & (S[A"-pp"; A"camlp4o"]);
         flag ["ocaml"; "pp_camlp4o"; "ocamldep"] & (S[A"-pp"; A"camlp4o"]);
         flag ["ocaml"; "pp_camlp4o"; "doc"]      & (S[A"-pp"; A"camlp4o"]);

    | _ -> ()
;;

dispatch 
  begin
    function
      | After_rules ->
          dispatch_ocamlfind After_rules;
          (* This first statement is not effective *)
          flag ["dep"; "pkg_camlp4.macro"; "has_gettext"] 
            & S[A"-ppopt"; A"-D";  A"-ppopt"; A"HAS_GETTEXT"];
          flag ["compile"; "pkg_camlp4.macro"; "has_gettext"] 
            & S[A"-ppopt"; A"-D";  A"-ppopt"; A"HAS_GETTEXT"];

      | e ->
          dispatch_ocamlfind e
  end
;;
(** END COPIED FROM src/ocamlbuild/myocamlbuild *)
