## Help spread the word

The best way to help the OASIS project is to use it, of course. From the
beginning, OASIS has been designed using real-life projects as examples to
define features. The first public version was the one matching the most
common requirements of the author's projects. It is now time to expand these
requirements to your projects as well.

* [Try OASIS](QUICKSTART.md) on your libraries/programs
* If it doesn't work out of the box,
  [you can try to fix the generated files](MANUAL.mkd#customization-of-generated-files)
* If you are able to solve the problem by yourself, send a "pull request" for inclusion

We really appreciate getting your feedback. Use [bug reports] or [feature requests].

  [bug reports]: https://github.com/ocaml/oasis/issues?q=is%3Aissue+is%3Aopen+label%3Abug
  [feature requests]: https://github.com/ocaml/oasis/issues?q=is%3Aissue+is%3Aopen+label%3Aenhancement

## Get involved into development

You can browse the source code of the [project].

  [project]: https://github.com/ocaml/oasis

To get a copy of the source tree, you must use git anonymously:

    $> git clone git://github.com/ocaml/oasis.git

Once you have the source code, you can pick a [bug] or a [feature request] to fix.

  [bug]: https://github.com/ocaml/oasis/issues?q=is%3Aissue+is%3Aopen+label%3Abug
  [feature request]: https://github.com/ocaml/oasis/issues?q=is%3Aissue+is%3Aopen+label%3Aenhancement

You can send back patches using git.  Create an account on
[github](https://github.com/), fork the
[project](https://github.com/ocaml/oasis), push your changes to your
github copy and submit a "Pull Request".  To push your changes, if
your github user is USERNAME, first add a new remote location to your
local repository (you have to do that once only):

    $> git remote add github git@github.com:USERNAME/oasis.git

and then push your master branch to it:

    $> git push github master

To update the source code with the latest available version:

    $> git pull

Further documentation about git can be found on
[git-scm.com](http://git-scm.com/).  See also
[Github help](https://help.github.com/).

## Write plugins and tools

If your requirements cannot be fulfilled by the actual code, you can extend
OASIS features through plugins. For now, plugins development need to be done
with the OASIS source tree.

* Get a copy of the OASIS source tree (see next section)
* Add a directory into `src/plugins/`
* Add an "open" statement in `src/OASISBuiltinPlugins.ml`
* Create a test into `test/data` and call it from within a test case (see
  `test/TestFull.ml`)
* Create your plugin

Writing tools is not yet very easy. You need to depend only on the content of
`src/oasis`, which should become a library soon. These files contain
everything needed to parse and extract information from an `_oasis` file.