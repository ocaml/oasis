
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


Creating plugins
================

TODO: complete

There are two kind of plugin:

 * command line plugins: they create subcommand for the command line interface
   of `oasis`. For example, the subcommand `oasis install pkg_name` is provided
   by the plugin `install`.
 * plugin that are used in `_oasis`: these plugins help to process the content
   of an `_oasis` file. They generate additional content in `setup.ml` and so on.
   You don''t have to load them, they will be automatically loaded when they are
   found in the `_oasis` file. For example `Plugins: AutoGenerate (0.1)` in the
   `_oasis` file will trigger the load of the plugin `autogenerate`.

The plugin system of `oasis` use findlib to detect plugins, so you must install
plugins as a standard OCaml library with a META file. OASIS detects plugin when
they have a field `plugin_system = "oasis"` or `plugin_system = "oasis-cli"` in
their META file.

Plugins are loaded as needed. You can ignore plugins with the command line options
`-ignore-plugins`.

Cut a release
=============

 * Check status on [Jenkins][jenkins].
 * Update CHANGES.txt:
  * Date, first entry must be "Version X.Y.Z".
  * Major/Minor changes.
  * Thanks section.
 * Create [blog post][blog-post].
 * `make deploy`
 * Update Change Log in the Files section of forge:
  * Copy-paste last entry of CHANGES.txt.
  * Don''t Merge lines of para.
  * Check 'Preserve my pre-formatted text.'
  * Check the result.
 * Add new version to trackers ([Bugs][bugs-version]/[Patches][patches-version]).
 * Close all bugs in the CHANGES.txt on trackers ([1][bugs], [2][patches]).
 * Publish blog post with updated download links.
 * G+ announce as OASIS.
 * Reshare G+ post on OCaml community.
 * Send mail to caml-list.

 [jenkins]: http://deci.ovh.le-gall.net:8080/job/ocaml-oasis/
 [blog-post]: http://le-gall.net/sylvain+violaine/blog/admin/posts.php
 [bugs-version]: https://forge.ocamlcore.org/tracker/admin/index.php?add_opt=1&boxid=995&group_id=54&atid=291
 [patches-version]: https://forge.ocamlcore.org/tracker/admin/index.php?add_opt=1&boxid=1007&group_id=54&atid=293
 [bugs]: https://forge.ocamlcore.org/tracker/?atid=291&group_id=54&func=browse
 [patches]: https://forge.ocamlcore.org/tracker/?atid=293&group_id=54&func=browse
