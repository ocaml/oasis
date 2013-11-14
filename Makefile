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


CONFIGUREFLAGS += --override ocamlbuildflags -classic-display --enable-tests
# TODO: gettext doesn't play nice with dynrun.
#CONFIGUREFLAGS += $(if $(shell ocamlfind query gettext),--enable-gettext,--disable-gettext)

default: test

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

# Count number of lines
#

wc:
	find src/ -name "*.ml" | xargs wc -l

wc-setup:
	awk -f src/tools/setup-wc.awk setup.ml

.PHONY: wc wc-setup

# Headache target
#  Fix license header of file.

headache:
	find ./ \
	  -name _darcs -prune -false \
	  -o -name .git -prune -false \
	  -o -name .svn -prune -false \
	  -o -name _build -prune -false \
	  -o -name dist -prune -false \
	  -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config

.PHONY: headache

# Binary distribution
# TODO: delegate this to a jenkins builder.

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

.PHONY: bindist bindist-step2

# Source distribution
# TODO: consider removing in favor of deploy

dist:
#	Check consistency of versions
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

doc-dist: doc
	./doc-dist.sh

.PHONY: doc-dist

# Precommit target
#  Check style of code.

PRECOMMIT_ARGS= \
	    --exclude myocamlbuild.ml \
	    --exclude setup.ml \
	    --exclude README.txt \
	    --exclude INSTALL.txt \
	    --exclude Makefile \
	    --exclude configure \
	    --exclude _tags

precommit:
	@if command -v OCamlPrecommit > /dev/null; then \
	  OCamlPrecommit $(PRECOMMIT_ARGS); \
	else \
	  echo "Skipping precommit checks.";\
	fi

precommit-full:
	OCamlPrecommit --full $(PRECOMMIT_ARGS)

test: precommit

.PHONY: precommit

# Deploy target
#  Deploy/release the software.

# TODO: check that this can work.
deploy: headache
	# TODO: create a plugin to create documentation.
	# oasis doc-dist
	admin-gallu-deploy --verbose \
	  --forge_upload --forge_group oasis --forge_user gildor-admin
	# TODO: when oasis doc-dist will work, re-add.
	#  --forge_extra_file "dist/oasis-doc-$(shell oasis query version).tar.gz"
	# TODO: create a plugin to send announcement.
	# oasis announce
	admin-gallu-oasis-increment --setup_run --use_vcs

.PHONY: deploy
