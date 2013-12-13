################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2011-2013, Sylvain Le Gall                                    #
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

OCAML_GETTEXT_PACKAGE = oasis
LINGUAS=$(shell cat LINGUAS)
SOURCES=POTFILES

ODN_OPTIONS=-parser Camlp4OCamlParser.cmo \
	    -parser Camlp4OCamlParserParser.cmo \
	    $(shell ocamlfind query \
  	      -predicates syntax,preprocessor \
	      -format "-I %d %A" -r \
	      odn.without.syntax)

OCAML_GETTEXT=ocaml-gettext
OCAML_GETTEXT_EXTRACT_OPTIONS=--extract-default-option '$(ODN_OPTIONS)'
OCAML_GETTEXT_COMPILE_OPTIONS=
OCAML_GETTEXT_INSTALL_OPTIONS=
OCAML_GETTEXT_MERGE_OPTIONS=

BUILDPO=../_build/po/

POFILES=$(addsuffix .po,$(LINGUAS))
MOFILES=$(addsuffix .mo,$(LINGUAS))
POTFILE=$(OCAML_GETTEXT_PACKAGE).pot

all: install-buildpo

install: install-po

uninstall: uninstall-po

clean:: clean-po

%.mo: %.po
	$(OCAML_GETTEXT) --action compile $(OCAML_GETTEXT_COMPILE_OPTIONS)    \
	  --compile-output $@ $^

%.pot: $(SOURCES)
	$(OCAML_GETTEXT) --action extract $(OCAML_GETTEXT_EXTRACT_OPTIONS)    \
	  --extract-pot $@ $^

%.po: $(POTFILE)
	$(OCAML_GETTEXT) --action merge   $(OCAML_GETTEXT_MERGE_OPTIONS)      \
	  --merge-pot $(POTFILE) $@

$(BUILDPO):
	mkdir -p $(BUILDPO)

.PRECIOUS: $(POTFILE)

install-buildpo: $(MOFILES) $(BUILDPO)
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	  --install-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	  --install-destdir $(BUILDPO) $(MOFILES)

install-po: $(MOFILES)
	$(OCAML_GETTEXT) --action install $(OCAML_GETTEXT_INSTALL_OPTIONS)    \
	  --install-textdomain $(OCAML_GETTEXT_PACKAGE)                       \
	  --install-destdir $(PODIR) $(MOFILES)

uninstall-po:
	$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS)  \
	  --uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                     \
	  --uninstall-orgdir $(PODIR) $(MOFILES)

clean-po:
	-$(OCAML_GETTEXT) --action uninstall $(OCAML_GETTEXT_INSTALL_OPTIONS) \
	  --uninstall-textdomain $(OCAML_GETTEXT_PACKAGE)                     \
	  --uninstall-orgdir $(BUILDPO) $(MOFILES)
	-$(RM) $(MOFILES)

