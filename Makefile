################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2011-2016, Sylvain Le Gall                                    #
#  Copyright (C) 2008-2011, OCamlCore SARL                                     #
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

OMAKEFLAGS:=$(shell (command -v omake > /dev/null) && echo --enable-omake-tests)
CONFIGUREFLAGS += --override ocamlbuildflags -classic-display \
                  --enable-tests \
                  --enable-devel \
                  --enable-devel-tests \
                  $(OMAKEFLAGS)

default: test

#TESTFLAGS=-only-test "OASIS:1:OASIS:2:File:12:test-freeform.oasis"
TIMINGS=1

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

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

configure:
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
	# TODO: reactivate when headache in opam will be ready.
	#find ./ \
	#  -name _darcs -prune -false \
	#  -o -name .git -prune -false \
	#  -o -name .svn -prune -false \
	#  -o -name _build -prune -false \
	#  -o -name dist -prune -false \
	#  -o -name '*[^~]' -type f \
	#  | xargs /usr/bin/headache -h _header -c _headache.config

.PHONY: headache

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
	-@if command -v OCamlPrecommit > /dev/null; then \
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

deploy: headache
	$(MAKE) doc
	dispakan $(DEPLOY_FLAGS)
	admin-gallu-oasis-increment
	$(MAKE) build
	./Main.native setup
	$(MAKE) distclean
	$(MAKE) test
	git commit -am "Update OASIS version."


.PHONY: deploy

# Create dev tarball.
#  Create a tarball for rebuilding the current version

dist-dev: build
	./Dist.byte -dev

.PHONY: dist-dev
