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

default: TESTFLAGS=-long
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

headache:
	find ./ -name _darcs -prune -false -o -name _build -prune \
	  -o -name ext -prune -false -o -type f \
	  | xargs headache -h _header -c _headache.config
