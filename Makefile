################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2008-2010, OCamlCore SARL                                     #
#                                                                              #
#  This library is free software; you can redistribute it and/or modify it     #
#  under the terms of the GNU Lesser General Public License as published by    #
#  the Free Software Foundation; either version 2.1 of the License, or (at     #
#  your option) any later version, with the OCaml static compilation           #
#  exception.                                                                  #
#                                                                              #
#  This library is distributed in the hope that it will be useful, but         #
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  #
#  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          #
#  details.                                                                    #
#                                                                              #
#  You should have received a copy of the GNU Lesser General Public License    #
#  along with this library; if not, write to the Free Software Foundation,     #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               #
################################################################################


CONFIGUREFLAGS += --override ocamlbuildflags -classic-display

CONFIGUREFLAGS += $(if $(shell ocamlfind query gettext),--enable-gettext,--disable-gettext)

default: test
	-OCamlDarcsPrecommit

#TESTFLAGS      += -long
#TESTFLAGS      += -verbose
#TESTFLAGS      += -debug
#TESTFLAGS      += -only-test OASIS:5:TestFull:20:data/bug982:0:standard

# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

# Backup targets to be able to build even if OASIS fails

wc:
	find src/ -name "*.ml" | xargs wc -l

headache:
	find ./ -name _darcs -prune -false -o -name _build -prune \
	  -false -o -name ext -prune -false -o -name bindist -prune \
	  -false -o -name boot -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: wc headache

# Binary distribution

BINDIST_DEBUG=false

bindist:
	if ! $(BINDIST_DEBUG); then $(SETUP) -distclean; fi
	$(SETUP) -configure
	$(MAKE) bindist-step2 BINDIST_CUSTOM=true

-include setup.data
BINDISTDIR=$(CURDIR)/bindist
BINDIR=$(BINDISTDIR)/bin-$(system)-$(architecture)


ifeq ($(os_type),"Win32")
tr_path = cygpath -w
else
tr_path = echo
endif

BINDISTGZ=$(pkg_name)-$(pkg_version)-bindist.tar.gz

bindist-step2:
	if ! $(BINDIST_DEBUG); then $(SETUP) -distclean; fi
	if test -d $(BINDISTDIR); then $(RM) -r $(BINDISTDIR); fi
	if test -e $(BINDISTGZ); then tar xzf $(BINDISTGZ); fi
	mkdir -p "$(BINDISTDIR)"
	mkdir -p "$(BINDISTDIR)/share/doc"
	mkdir -p "$(BINDIR)"
	$(SETUP) -configure \
	  --prefix "$$($(tr_path) $(BINDISTDIR))" \
	  --bindir "$$($(tr_path) $(BINDIR))" \
	  --docdir "$$($(tr_path) $(BINDISTDIR)/share/doc)" \
	  --disable-libraries \
	  --override ocamlbuildflags "-classic-display -tag custom" \
	  $(CONFIGUREFLAGS)
	$(SETUP) -build
	if ! $(BINDIST_DEBUG); then $(SETUP) -test; fi
	if ! [ "$(os_type)" = "Win32" ]; then $(SETUP) -doc; fi
	$(SETUP) -install
	tar czf $(BINDISTGZ) bindist
	-$(RM) -r $(BINDISTDIR)

.PHONY: build-backup test-backup clean-backup wc headache bindist bindist-step2

# Source distribution

dist:
	# Check consistency of versions
	OASIS_CMD_VER=$$(oasis version); \
	OASIS_DIST_VER=$$(oasis query version); \
	OASIS_SETUP_VER=$$($(SETUP) -version); \
	if [ "x$$OASIS_CMD_VER" != "x$$OASIS_DIST_VER" ]; then \
		echo "Must be running the same version of oasis as the one being built" >&2; \
		exit 1; \
	fi; \
	if [ "x$$OASIS_SETUP_VER" != "x$$OASIS_DIST_VER" ]; then \
		echo "Must be running the have used the same version for setup.ml  as the one being built" >&2; \
		exit 1; \
	fi
	./src/tools/oasis-dist.ml

.PHONY: dist

# Fixing permissions

fixperms:
	for i in $$(cat _fixperms); do chmod +x "./$$i"; done

.PHONY: fixperms
