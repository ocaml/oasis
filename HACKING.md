
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

There are two kind of plugin:

 * command line plugins: they create subcommand for the command line interface
   of `oasis`. For example, the subcommand `oasis install pkg_name` is provided
   by the plugin `install`.
 * plugin that are used in `_oasis`: these plugins help to process the content
   of an `_oasis` file. They generate additional content in `setup.ml` and so on.
   You don't have to load them, they will be automatically loaded when they are
   found in the `_oasis` file. For example `Plugins: AutoGenerate (0.1)` in the
   `_oasis` file will trigger the load of the plugin `autogenerate`.

The plugin system of `oasis` use findlib to detect plugins, so you must install
plugins as a standard OCaml library with a META file. OASIS detects plugin when
they have a field `plugin_system = "oasis"` or `plugin_system = "oasis-cli"` in
their META file.

Plugins are loaded as needed. You can ignore plugins with the command line options
`-ignore-plugins`.
