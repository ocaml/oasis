# The OMake plugin

Author: Gerd Stolpmann, gerd@gerd-stolpmann.de



The OMake plugin is meant as a powerful alternative to the ocamlbuild
plugin. Its special feature is that the user can modify the generated
OMakefiles, and add further build rules that are beyond the scope of
OASIS.

The OMake plugin uses a scheme that is largely compatible with the way
ocamlbuild works. However, there are a few differences you should be
aware of (see below in this doc). That means it is normally sufficient
to change the plugin type in your _oasis file to "omake". At this
point, generate the OMakefiles with `oasis setup`, modify them by your
needs, and enjoy the new scripting capabilities.

What is included:

 * A new build plugin
 * A new doc plugin
 * A new install plugin

There is no new configuration plugin. The standard one coming with OASIS
is good enough, and if you need to run some custom configuration script,
you can still do with a PostConfCommand.

The new install plugin is optional. You can also use the standard install
plugin of OASIS. The new install plugin is easier to extend, though,
because it also runs OMake.

## Switching to OMake

Just change:

 * In all sections for libraries and executables (or just globally):
   ```
   BuildType: OMake
   ```

 * If you want to use the install plugin:
   ```
   InstallType: OMake
   ```

 * In documents, change:
   ```
   Type: OMake
   ```

   Also, use
     `XOMakePath` and `XOMakeLibraries`
   instead of
     `XOCamlbuildPath` and `XOCamlbuildLibraries`

After that, don't forget to run `oasis setup`. This generates a bunch
of files. After that, you are ready to go, and e.g.

```
ocaml setup.ml -build
```

will invoke OMake. Note that you can also run OMake directly:

```
omake build
omake doc
omake install
```

When distributing your project, you should also pack up the generated files
(`OMakeroot`, `OMakefile`, `_oasis_*.om` except `_oasis_setup.om`).

## Generated files

After `oasis setup`, the following files are generated:

 * OMakeroot: This file marks the root of the directory hierarchy, and
   contains global OMake configurations. This file is constant, and never
   changed by `oasis setup` again once put into place.

 * OMakefile: This file exists in every directory of the hierarchy,
   and is starting point for the definition of rules for the
   directory.  This file is constant, and never changed by `oasis setup`
   again once put into place. The idea is that this file
   can be freely modified by your needs.

 * _oasis_lib.om: This is the library with additional OMake functionality.
   It is overwritten with every `oasis setup`.

 * _oasis_hier.om: This file defines a variable with the subdirectories.
   It is overwritten with every `oasis setup`.

 * _oasis_build.om: This file defines the build rules derived from _oasis.
   It is overwritten with every `oasis setup`.

 * _oasis_install.om: This file defines the install rules derived from _oasis.
   It is overwritten with every `oasis setup`.

Note that OMake stores a binary version of the *.om files with suffix .omc.

## Configure

OMake is able to read setup.data (the file where the result of
`ocaml setup.ml -configure` is written to). There is a converted file
_oasis_setup.om:

 * All OASIS variables are prefixed with "oasis_". E.g. "bindir" is
   available as "oasis_bindir".

 * A couple of extra configurations are appended to _oasis_setup.om,
   e.g. the C compiler to use (CC) is extracted from OASIS variables.

The file _oasis_setup.om is included into the OMakefile in the root
directory. If you need to adjust variables, you can do that after the
"include _oasis_setup.om" line.

Note that _oasis_setup.om is automatically rebuilt once setup.data is
changed.

## Build

OMake doesn't use a separate build directory. Object files are put
into the source hierarchy side by side with the source.

(Note: there is a vmount() function, but it is still marked as
experimental feature. See the OMake manual.)

There is one thing that is fundamentally different. First, there is no
_tags file. OMake only gets the build flags from the OMakefile. Second,
OMake takes the build flags from the OMakefile in the same directory as
the module/library/executable. The build flags are derived from a number
of variables defined in the OMakefiles. The exact set is documented in
the OMake manual, but the most important variables are:

   Variable        | Meaning
-------------------|---------------------------------------------
   OCAMLPACKS:     | the findlib packages
   OCAMLINCLUDES:  | other project directories to include
   OCAMLFLAGS:     | flags for both ocamlc and ocamlopt
   OCAMLCFLAGS:    | flags for ocamlc
   OCAMLOPTFLAGS:  | flags for ocamlopt
   OCAMLFINDFLAGS: | flags for ocamlfind

The generated _oasis_build.om files already initialize these variables
in a meaningful way. Nevertheless, you should know:

 * For compiling modules, the value of of these variable at the end
   of the OMakefile in the same directory counts.

 * If you want good control over the flags for a certain library or
   executable, build the library or executable in a directory of its
   own. This way, you can be sure that it is isolated from the
   settings of any other library or executable, as these are built
   in different directories.

 * It is a bad idea to put modules into subdirectories unless you want
   to achieve a special effect. For example, you could have in _oasis:

   ```
   Modules:
      P
      Q
      subdir/R
      subdir/S
   ```

   The point here is now that R and S are now built in the subdirectory,
   and the flags are taken from the OMakefile in this subdirectory. The
   generator does not take care of anything in this situation, and probably
   does not generate the right flags.

   So, don't do this even if you only have something innocent in mind
   like grouping your files logically.

 * It is possible to set deviating flags for a module. There are a number
   of helper functions to do so. E.g. if a module X needs camlp4 syntax,
   the way to enable this only for this module is:

   ```
   OASIS_modify_OCAMLFINDFLAGS(X, -syntax camlp4)
   ```

   Put that into the OMakefile of the directory where X exists.

There is some documentation in the top-most OMakefile explaining further
ways to set flags.

## Doc

The doc plugin understands a couple of additional options in _oasis:

 * OMakePath: where to build the document
 * OMakeLibraries: libraries to include into the document
 * OMakeModules: modules to include into the document
 * OMakeTexts: texts to include (the files must have suffix .txt
   but omit the extension, e.g. "OMakeTexts: foo" would take foo.txt)
 * OMakeIntro: introductory text (also without extension)
 * OMakeFlags: additional flags for ocamldoc

You can set Format to either HTML, PDF, PostScript, or DVI.

## Install

The new install plugin should just do the same as the internal plugin,
only that the installation is done via OMake rules.

If you e.g. want to run some additional commands, you can modify the
install-here rule in the OMakefile in the directory where you want
to install something. It normally reads:

```
install-here: $(INSTALL_TARGETS)
```

without commands, and you can simply add some commands, e.g.

```
install-here: $(INSTALL_TARGETS)
    cp myfile /dest/dir/
```

In order to comply with the configuration, you should copy the files
into the right directories, e.g.

```
install-here: $(INSTALL_TARGETS)
    cp myconfig $(OASIS_destdir $(oasis_sysconfdir))
```

Here, the variable oasis_sysconfdir is the configured directory for
configurations, and the function OASIS_destdir prepends, if configured,
the destination prefix (-destdir). (NB. OASIS_destdir is defined in
_oasis_lib.om.)

## Uninstalling

This works also the same as for the internal plugin. Note that OMake
keeps track of the installed files in setup.log.om.
