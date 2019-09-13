<!--- OASIS_START --->
<!--- DO NOT EDIT (digest: ca4f8ffc6b65cf94ef69d514a9ba3495) --->

This is the INSTALL file for the oasis distribution.

This package uses OASIS to generate its build system. See section OASIS for
full information.

Dependencies
============

In order to compile this package, you will need:

* ocaml (>= 3.12.1) for all, test devel, test main, test main_loader,
  test omake, test quickstart, doc api-oasis, doc manual
* findlib (>= 1.3.1)
* make for all, test devel, test main, test main_loader, test omake,
  test quickstart, doc api-oasis, doc manual
* ocamlmod for library base, library oasis, test main, test omake
* ocamlify for library base, library oasis
* ocamlbuild for library builtin-plugins
* oUnit (>= 2.0.0) for library test-common, executable test-devel,
  executable test-main, executable test-omake, executable test-quickstart,
  executable test_loader
* fileutils (>= 0.4.2) for library test-common, executable test-main,
  executable test-omake, executable test-quickstart, executable test_loader
* pcre for library test-common
* expect (>= 0.0.4) for executable test-quickstart
* benchmark (>= 1.2) for executable bench
* omake for test omake

Installing
==========

1. Uncompress the source archive and go to the root of the package
2. Run 'ocaml setup.ml -configure'
3. Run 'ocaml setup.ml -build'
4. Run 'ocaml setup.ml -install'

Uninstalling
============

1. Go to the root of the package
2. Run 'ocaml setup.ml -uninstall'

OASIS
=====

OASIS is a program that generates a setup.ml file using a simple '_oasis'
configuration file. The generated setup only depends on the standard OCaml
installation: no additional library is required.

<!--- OASIS_STOP --->
