default: test

test: all
	cd '$(CURDIR)/test' && ../_build/test/test.byte $(TESTFLAGS)

.PHONY: test

# Default target
all:
	ocamlbuild -classic-display ocaml-autobuild.otarget

clean:
	-ocamlbuild -classic-display -clean
