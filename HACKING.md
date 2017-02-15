
Guidelines for developing OASIS
===============================

Updates in `_oasis`:

`_oasis` and setup.ml of the OASIS project need to be updated with the officially
released version of OASIS.

__You should never use the version of OASIS under development to update__

Go to http://oasis.forge.ocamlcore.org/ and install the latest available
version.

The reason for that decision is that if you generate a new version of setup.ml
using your current version, each commit will include a change in setup.ml and
it will be mostly a repetition of something you wrote elsewhere in your commit.
Moreover, it can lead to conflict and it is never good to have to worry about
conflicts (it has generated irreconcilable changes when the project was using
darcs).


Creating plugins
================

There are two kind of plugin:

 * oasis command line plugins (oasis-cli): they create subcommand for the
   command line interface of `oasis`. For example, the subcommand `oasis
   print-hello` is provided by the plugin `print-hello`.
   [Example](examples/plugins/oasis-plugin-print-hello)
 * oasis plugins (oasis): they are used in `_oasis`: these plugins help to
   process the content of an `_oasis` file and generates additional content in
   the target package.
   [Example](examples/plugins/oasis-plugin-versionfile),
   [Example usage](examples/plugins/with-plugin-versionfile)


`oasis`'s plugin system uses findlib to detect plugins, so you must install
plugins as a standard OCaml library with a META file. OASIS detects plugins when
they have a field `plugin_system = "oasis"` or `plugin_system = "oasis-cli"` in
their META file.

Plugins are loaded as needed. You can ignore plugins with the command line
options `-ignore-plugins`. 'oasis' plugins are loaded if they appears in a
plugin field of an `_oasis` file. 'oasis-cli' plugins iare loaded when you
invoke their name on the command line.


Cut a release
=============

 * Check status on [Jenkins][jenkins], [Travis][travis] and
   [AppVeyor][appveyor].
 * Run headache.
 * Update CHANGES.txt:
  * git log 0.4.2..HEAD >> CHANGES.txt
  * Date, first entry must be "Version X.Y.Z".
  * Major/Minor changes.
  * Features section.
  * Thanks section.
 * Create a [blog post][blog-post].
  * See the [blog post example][blog-post-example] for content
 * Create a [news on the forge][forge-post].
  * Subject: OASIS v0.4.2 release
  * Details: Read the full blog post here: URL of the blog post.
 * Change version in `\_oasis`, drop the ~HEAD, rerun `oasis setup` and commit.
 * `make deploy`
 * Make sure the new version is X.Y.Z~HEAD and maybe rerun update.
 * Update Change Log in the Files section of forge:
  * Copy-paste last entry of CHANGES.txt, not including the first line
    (date...).
  * Don''t Merge lines of para.
  * Check 'Preserve my pre-formatted text.'
  * Check the result.
 * Add new version to trackers ([Bugs][bugs-version]/[Pull request][pull-version]).
 * Create an OPAM package using [oasis2opam][oasis2opam].
  * Use the URL of the uploaded tarball.
 * Close all bugs in the CHANGES.txt on trackers ([1][bugs]).
 * Publish blog post with updated download links.
 * G+ announce as OASIS.
 * Reshare G+ post on OCaml community.
 * Send mail to caml-list, oasis-devel.

 [jenkins]: http://deci.ovh.le-gall.net:8080/job/ocaml-oasis/
 [travis]: https://travis-ci.org/ocaml/oasis
 [appveyor]: https://ci.appveyor.com/project/gildor478/oasis
 [blog-post]: https://le-gall.net/sylvain+violaine/blog/admin/posts.php
 [blog-post-example]: http://le-gall.net/sylvain+violaine/blog/index.php?post/2014/10/23/Release-of-OASIS-0.4.5
 [forge-post]: https://forge.ocamlcore.org/news/submit.php?group_id=54
 [bugs-version]: https://github.com/ocaml/oasis/issues/new
 [pull-version]: https://github.com/ocaml/oasis/pulls
 [bugs]: https://github.com/ocaml/oasis/issues
 [oasis2opam]: https://github.com/ocaml/oasis2opam


Versions support policy
=======================

To help keep OASIS as widely usable as possible, we should be conservative about
the minimum required version of our dependencies. Here are some policies, to
determine what version of a dependency we should require:

 * For generated setup.ml:
  * No deps (standalone) except OCaml
  * OCaml version must be at least the one in Debian stable.
 * For generated files (e.g. myocamlbuild.ml):
  * Version of the target in Debian stable or that matches the constraint
    expressed in `_oasis`. E.g. if OCamlVersion: >= 4.01, we can generate
    myocamlbuild.ml for this specific version because the constraint will be
    checked at configure time.
 * For the OASIS sources:
  * OCaml version in Debian stable.
  * All deps must be in Debian stable.
  * Exception for related projects: ocamlmod, ocamlify, ocaml-data-notation
 * For the OASIS tests:
  * Version published, no strong requirement since tests can be disabled.


Backwards compatibility
=======================

OASIS supports the former version of OASISFormat. Most of the important things
have been backported to former version. However in the long term we might decide
to drop support of old OASISFormat.

We will support a version of OASISFormat for as long as possible.  However, we
may drop support for any OASISFormat version older than the version of OASIS in
Debian stable.

For example:
 * Debian stable is released with OASIS `0.2.0`
 * We will continue to support OASISFormat: `0.1` as long as possible but, at some
   point we will enforce using at least OASISFormat: `0.2`.

A list of supported OASISFormat can be found in OASISPackage_intern.ml.

Debian relationship
===================

We often use Debian, especially for version reference. OCaml support in Debian
has a long history and we need to pick one reference. Since one of the upstream
author is related to Debian, we made the decision to synchronize on Debian
stable release.
