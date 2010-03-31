default: test

HAS_GETTEXT=$(shell if ocamlfind query gettext > /dev/null 2>&1; then \
	              echo true; \
		    else \
		      echo false; fi)

OCAMLBUILDFLAGS=-tag debug -classic-display
ifeq ($(HAS_GETTEXT),true)
OCAMLBUILDFLAGS+=-tag has_gettext
endif

test: all
	cd '$(CURDIR)/test' && ../_build/test/test.byte $(TESTFLAGS)

.PHONY: test

# Default target
all:
	ocamlbuild $(OCAMLBUILDFLAGS) oasis.otarget
	cp _build/src/Main.byte _build/src/OASIS.byte
ifeq ($(HAS_GETTEXT),true)
	$(MAKE) -C po all
endif 

clean:
	-ocamlbuild -classic-display -clean
	-$(RM) doc/MANUAL.mkd
	$(MAKE) -C po clean

wc:
	find src/ -name "*.ml" | xargs wc -l

doc: all
	env LANG=C _build/src/OASIS.byte -documentation > doc/MANUAL.mkd

