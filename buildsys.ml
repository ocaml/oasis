
#use "src/base/buildsys.ml";;
#use "src/ocamlbuild/buildsys.ml";;

let () =
  Action.main
    "ocaml-autobuild"
    "0.0.0"
    (* Command line argument *)
    [
    ]
    (* Checks*)
    [ 
      Check.fenv (Check.package ~version_comparator:">= 1.0.1" "oUnit");
    ]
    (* .in files *)
    [
    ]
    (* Targets *)
    [
    ]
    (* Packs *)
    [
      OCamlbuild.package 
        ~ocamlbuild_targets:
        [
          "ocaml-autobuild.otarget";
        ];
    ]
;;

