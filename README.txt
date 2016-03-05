(* OASIS_START *)
(* DO NOT EDIT (digest: 850a4f92bc8d28b3dd7640aaec5892a2) *)

oasis - Architecture for building OCaml libraries and applications
==================================================================

OASIS generates a full configure, build and install system for your
application. It starts with a simple `_oasis` file at the toplevel of your
project and creates everything required.

It uses external tools like OCamlbuild and it can be considered as the glue
between various subsystems that do the job. It should support the following
tools:

 * OCamlbuild
 * OMake (todo)
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
