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


CONFIGUREFLAGS = --override ocamlbuildflags -classic-display

default: test
TESTFLAGS      += -long 
#TESTFLAGS      += -verbose
#TESTFLAGS      += -only-test OASIS:4:Basic:1

# OASIS_START
# DO NOT EDIT (digest: c670bbc06ab2e6f432b790475d6ad412)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test install uninstall clean distclean configure

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

# Binary distribution 

BINDIST_DEBUG=false

CP_OR_LN=cp
ifeq ($(BINDIST_DEBUG),true)
  CP_OR_LN=ln -sf
endif

bindist:
	if ! $(BINDIST_DEBUG); then $(SETUP) -distclean; fi
	$(SETUP) -configure
	$(MAKE) bindist-step2 BINDIST_CUSTOM=true

-include setup.data
BINDISTDIR=$(CURDIR)/bindist
BINDIR=$(BINDISTDIR)/bin-$(system)-$(architecture)


ifeq ($(os_type),"Win32")
gettextflags = --disable-gettext
tr_path = cygpath -w
else
gettextflags = 
tr_path = echo 
endif 

bindist-step2:
	if ! $(BINDIST_DEBUG); then $(SETUP) -distclean; fi
	if test -d $(BINDISTDIR); then $(RM) -r $(BINDISTDIR); fi
	mkdir -p "$(BINDISTDIR)"
	mkdir -p "$(BINDISTDIR)/share/doc"
	mkdir -p "$(BINDIR)"
	$(SETUP) -configure \
	  --prefix "$$($(tr_path) $(BINDISTDIR))" \
	  --bindir "$$($(tr_path) $(BINDIR))" \
	  --docdir "$$($(tr_path) $(BINDISTDIR)/share/doc)" \
	  --disable-libraries \
	  $(gettextflags) \
	  --override ocamlbuildflags "-classic-display -tag custom"
	$(SETUP) -build 
	if ! $(BINDIST_DEBUG); then $(SETUP) -test; fi
	if ! [ "$(os_type)" = "Win32" ]; then $(SETUP) -doc; fi
	$(SETUP) -install

.PHONY: build-backup test-backup clean-backup wc headache bindist bindist-step2
