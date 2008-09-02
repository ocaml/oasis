
OCAMLFLAGS=$(ocamlfind query -format "-I %d %a" -r -predicates byte,toploop $PACKAGES)

exec ocaml $OCAMLFLAGS $(dirname $0)/buildsys.ml $*
