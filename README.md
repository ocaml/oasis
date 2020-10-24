[![Travis-CI Build Status](https://travis-ci.org/ocaml/oasis.svg?branch=master)](https://travis-ci.org/ocaml/oasis)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/42gumiqt5le643t2?svg=true)](https://ci.appveyor.com/project/gildor478/oasis)
[![Join the chat at https://gitter.im/ocaml/oasis](https://badges.gitter.im/ocaml/oasis.svg)](https://gitter.im/ocaml/oasis?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<!--- OASIS_START --->
<!--- DO NOT EDIT (digest: fa5b2e44255b14951d927b0e054fa38f) --->

oasis - Tooling for building OCaml libraries and applications
=============================================================

<img src="https://raw.githubusercontent.com/ocaml/oasis/master/doc/images/logo.svg" width="64" height="64"/></img>

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
integrate your libraries and software with third parties tools like OPAM.

See the file [INSTALL.md](INSTALL.md) for building and installation
instructions.

[Bug reports](https://github.com/ocaml/oasis/issues)

Copyright and license
---------------------

(C) 2011-2016 Sylvain Le Gall
(C) 2008-2010 OCamlCore SARL

oasis is distributed under the terms of the GNU Lesser General Public License
version 2.1 with OCaml linking exception.

See [COPYING.txt](COPYING.txt) for more information.

<!--- OASIS_STOP --->

Documentation
-------------

* [OASIS Quickstart](doc/QUICKSTART.md) which show you how to set up
  OASIS for a single executable and library.
* Have a look at our [frequently asked questions](doc/FAQ.md)
* [OASIS User manual](doc/MANUAL.mkd)
* [Contributing](doc/CONTRIBUTE.md)
* [Release process](doc/HACKING.md)

Examples
--------

* [custom](https://github.com/ocaml/oasis/blob/master/examples/custom/_oasis)
 a project that uses the custom plugin and make to build
* [flags](https://github.com/ocaml/oasis/blob/master/examples/flags/_oasis)
 a project that uses flags and .ab files where these flags are substituted
* [interdepend-libraries](https://github.com/ocaml/oasis/blob/master/examples/interdepend-libraries/_oasis)
 a project that uses several libraries linked together
* [with-c](https://github.com/ocaml/oasis/blob/master/examples/with-c/_oasis)
 a project that uses C files in libraries and executables
* [with-data](https://github.com/ocaml/oasis/blob/master/examples/with-data/_oasis)
 a project that installs data files
* [with-subpackages](https://github.com/ocaml/oasis/blob/master/examples/with-subpackage/_oasis)
 a project that uses a syntax extension and a library
* [browse the examples](https://github.com/ocaml/oasis/tree/master/examples)
 directory of OASIS for other examples

Featured projects that use \_oasis:

* [ocamlify](https://github.com/gildor478/ocamlify):
  the [\_oasis](https://github.com/gildor478/ocamlify/blob/master/_oasis) file includes 2 syntax extensions,
  a library and tests

Related documentation
---------------------

[OMake README](src/plugins/omake/README.md)
