
# AUTOBUILD_START 

SETUP = ocaml setup.ml

BUILDFLAGS     =
DOCFLAGS       =
TESTFLAGS      =
CLEANFLAGS     =
DISTCLEANFLAGS =
CONFIGUREFLAGS =

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data
	$(SETUP) -test $(TESTFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data: configure
configure: 
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test clean distclean configure 

# AUTOBUILD_STOP 
