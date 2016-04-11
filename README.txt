(* OASIS_START *)
(* DO NOT EDIT (digest: 7cefd474e4af4a91067d668e9311a91b) *)

oasis - Tooling for building OCaml libraries and applications
=============================================================

OASIS generates a full configure, build and install system for your
application. It starts with a simple `_oasis` file at the toplevel of your
project and creates everything required.

OASIS leverages existing OCaml tooling to perform most of it's work. In fact,
it might be more appropriate to think of it as simply the glue that binds
these other subsystems together and coordinates the work that they do. It
should support the following tools:

 * OCamlbuild
 * OMake
 * OCamlMakefile (todo),
 * ocaml-autoconf (todo)

It also features a do-it-yourself command line invocation and an internal
configure/install scheme. Libraries are managed through findlib. It has been
tested on GNU Linux and Windows.

It also allows to have standard entry points and description. It helps to
integrates your libraries and software with third parties tools like GODI.

See the file [INSTALL.txt](INSTALL.txt) for building and installation
instructions.

[Home page](http://oasis.forge.ocamlcore.org/)

Copyright and license
---------------------

(C) 2008-2010 OCamlCore SARL

oasis is distributed under the terms of the GNU Lesser General Public License
version 2.1 with OCaml linking exception.

See [COPYING.txt](COPYING.txt) for more information.

(* OASIS_STOP *)
