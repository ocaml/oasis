default: test

test: all
	cd '$(CURDIR)/test' && ../_build/test/test.byte $(TESTFLAGS)

.PHONY: test

# Default target
all:
	ocamlbuild -classic-display ocaml-autobuild.otarget

clean:
	-$(RM) examples/flags/myocamlbuild.ml
	-$(RM) examples/flags/setup.ml
	-$(RM) examples/flags/setup.data
	-$(RM) examples/flags/Makefile
	-$(RM) examples/flags/src/simplelibext/simplelibext.mllib
	-$(RM) examples/flags/src/simplelib/simplelib.mllib
	-$(RM) examples/flags/src/simplelib/Conf.ml
	-$(RM) examples/flags/flags.itarget
	-$(RM) examples/simplelib/myocamlbuild.ml
	-$(RM) examples/simplelib/setup.ml
	-$(RM) examples/simplelib/setup.data
	-$(RM) examples/simplelib/Makefile
	-$(RM) examples/simplelib/src/simplelib.mllib
	-ocamlbuild -classic-display -clean
