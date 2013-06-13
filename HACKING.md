
Guidelines to develop OASIS
===========================

Updates in _oasis:

_oasis and setup.ml of the OASIS project need to be updated with the officialy
released version of OASIS.

__You should never use the version of OASIS under development to update__

Go to http://oasis.forge.ocamlcore.org/ and install the latest available
version.

The reason of that decision is that if you generate a new version of setup.ml
using your current version, each commit will include a change in setup.ml and
it will be mostly a repetition of something you write elsewhere in your commit.
Moreover, it can lead to conflict and it is never good to have to worry about
conflicts (it has generated unreconciliable changes when the project was using
darcs).
