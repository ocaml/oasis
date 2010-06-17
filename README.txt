(* OASIS_START *)
(* DO NOT EDIT (digest: 737cf163d52c0c0a505a7d3b691deff4) *)
This is the README file for the oasis distribution.

(C) 2008-2010 OCamlCore SARL

Architecture for building OCaml libraries and applications

OASIS generates a full configure, build and install system for
yourapplication. It starts with a simple `_oasis` file at the toplevel of
yourproject and creates everything required..It uses external tools like
OCamlbuild and it can be considered as the gluebetween various subsystems
that do the job. It should support the followingtools:- OCamlbuild- OMake
(todo)- OCamlMakefile (todo),- ocaml-autoconf (todo).It also features a
do-it-yourself command line invocation and an internalconfigure/install
scheme. Libraries are managed through findlib. It has beentested on GNU Linux
and Windows..It also allows to have standard entry points and description. It
helps tointegrates your libraries and software with third parties tools like
GODI.

See the files INSTALL.txt for building and installation instructions. See the
file COPYING.txt for copying conditions. 

Home page: http://oasis.forge.ocamlcore.org/


(* OASIS_STOP *)
