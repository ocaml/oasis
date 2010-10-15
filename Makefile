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
#TESTFLAGS      += -long 
#TESTFLAGS      += -verbose
#TESTFLAGS      += -debug
#TESTFLAGS      += -only-test OASIS:5:TestFull:0:../examples/flags:1

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

OASIS ?= boot/OASIS

build-backup:
	$(OASIS)
	touch setup.data
	ocamlbuild $(OCAMLBUILDFLAGS) src/tools/ocamlmod.byte
	cp _build/src/tools/ocamlmod.byte _build/src/tools/ocamlmod
	ocamlbuild $(OCAMLBUILDFLAGS) \
	  src/oasis/oasis.cma \
	  src/base/base.cma \
	  src/plugins/custom/plugin-custom.cma \
	  src/plugins/none/plugin-none.cma \
	  src/plugins/internal/plugin-internal.cma \
	  src/plugins/ocamlbuild/plugin-ocamlbuild.cma \
	  src/plugins/extra/META/plugin-meta.cma \
	  src/plugins/extra/devfiles/plugin-devfiles.cma \
	  src/plugins/extra/stdfiles/plugin-stdfiles.cma \
	  src/builtin-plugins.cma \
	  src/OASISMain.byte test/test.byte
	cp _build/src/OASISMain.byte _build/src/OASIS
	cp _build/test/test.byte _build/test/test


TEST_BACKUP_RECURSE ?= true

test-backup: build-backup
	if cd test && ../_build/test/test $(TESTFLAGS); then \
	  cd ..; \
	  if $(TEST_BACKUP_RECURSE) && \
	     $(MAKE) test-backup \
	       TEST_BACKUP_RECURSE=false \
	       OASIS=_build/src/OASIS; then \
	    cp boot/OASIS boot/OASIS.old; \
	    cp _build/src/OASIS boot/OASIS; \
	  fi; \
	fi

clean-backup:
	ocamlbuild -clean

wc:
	find src/ -name "*.ml" | xargs wc -l

headache:
	find ./ -name _darcs -prune -false -o -name _build -prune \
	  -false -o -name ext -prune -false -o -name bindist -prune -false \
	  -o -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: build-backup test-backup clean-backup wc headache

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

dist: setup.data
	if ! [ "$$(darcs diff | wc -l)" = 0 ]; then \
	  echo E: Uncommited changes >&2 ; exit 1; \
	fi
	$(MAKE) test
	$(MAKE) dist-step2

-include setup.data
dist-step2:
	darcs dist --dist-name $(pkg_name)-$(pkg_version)
	if ! (darcs query tag | grep "$(pkg_version)" > /dev/null); then \
	  darcs tag "$(pkg_version)"; \
	else \
	  echo W: Version $(pkg_version) already tagged >&2; \
	fi 
	gpg -s -a -b "$(pkg_name)-$(pkg_version).tar.gz"

.PHONY: dist dist-step2
