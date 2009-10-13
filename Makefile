default: test

test: all
	cd '$(CURDIR)/test' && ../_build/test/test.byte $(TESTFLAGS)

.PHONY: test

# Default target
all:
	ocamlbuild -classic-display ocaml-autobuild.otarget

EXAMPLES=examples/flags examples/simplelib examples/findlib examples/custom
STD_FILES=myocamlbuild.ml setup.ml setup.date Makefile configure _tags

GENERATED_FILES += $(foreach example,$(EXAMPLES), $(addprefix $(example)/,$(STD_FILES)))

clean:
	-$(RM) $(GENERATED_FILES)
	-$(RM) examples/flags/src/simplelibext/simplelibext.mllib
	-$(RM) examples/flags/src/simplelib/simplelib.mllib
	-$(RM) examples/flags/src/simplelib/Conf.ml
	-$(RM) examples/simplelib/src/simplelib.mllib
	-$(RM) examples/findlib/src/simplelib.mllib
	-ocamlbuild -classic-display -clean
