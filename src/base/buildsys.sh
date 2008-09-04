
OCAMLFLAGS=$(ocamlfind query -format "-I %d %a" -separator " " -suffix "" -r -predicates byte,toploop $PACKAGES)

exec ocaml $OCAMLFLAGS $(dirname $0)/buildsys.ml $*
