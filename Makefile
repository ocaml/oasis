
OCAMLBUILDFLAGS += -classic-display 

all:
	ocamlbuild $(OCAMLBUILDFLAGS) ocaml-autobuild.otarget

clean:
	ocamlbuild $(OCAMLBUILDFLAGS) -clean
