default: test  

test: all
	cd '$(CURDIR)/test' && ../_build/test/test.byte $(TESTFLAGS)

.PHONY: test

# Default target
all:
	ocamlbuild -tag debug -classic-display oasis.otarget
	cp _build/src/Main.byte _build/src/OASIS.byte
	$(MAKE) -C po all

clean:
	-ocamlbuild -classic-display -clean
	-$(RM) doc/README.mkd
	$(MAKE) -C po clean
	$(MAKE) -C doc clean

wc:
	find src/ -name "*.ml" | xargs wc -l

doc: all
	env LANG=C _build/src/OASIS.byte -documentation > doc/README.mkd
	$(MAKE) -C doc

