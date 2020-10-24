This document shows you how to quickly start an OASIS project. We choose the most easy 
way here but there are many more possible options that you can use. Read the
[user manual](MANUAL.mkd) for a more in-depth understanding.

# Install OASIS

The recommended way to install oasis is via the [opam package manager][opam]:

[opam]: https://opam.ocaml.org

```sh
$ opam install oasis
```

# The `_oasis` file

The central point of OASIS is the `_oasis` file that should be placed in the toplevel
directory of your project. 

This file can be created by hand, but OASIS provide a simple generator for it. Use the 
command `oasis quickstart` to launch it. The questions are localized, maybe the output
of the program will be in your language. 

Answer each question and end your input with the `enter` key. If you have a problem
you can ask for help with `? + enter`. If a default value exists, it will be
displayed and in this case just hit `enter` to use it.

# Create an executable

I start with a very simple project: 

    $> echo 'let () = print_endline "Hello world!"' > main.ml

This first commmand creates the only OCaml source file we need.

    $> oasis quickstart

    Creating _oasis file

    Value for field 'Name'? (type '?' for help) helloworld

    Value for field 'Version'? (type '?' for help) 0.1

    Value for field 'Synopsis'? (type '?' for help) Hello world with OCaml and OASIS

    Value for field 'Authors'? (type '?' for help) Sylvain Le Gall

    Value for field 'License'? (type '?' for help) LGPL

    # Here we ask for help

    Create a section? (default is 'n', type '?' for help) ?
    n: stop
    l: create a library
    e: create an executable
    f: create a flag
    s: create a source repository
    t: create a test

    Create a section? (default is 'n', type '?' for help) e

    Executable name? helloworld

    Value for field 'Path'? (type '?' for help) .

    Value for field 'MainIs'? (type '?' for help) main.ml

    Create another section? (default is 'n', type '?' for help) n

Let take a look at the result:

    $> cat _oasis 
    OASISFormat: 0.1
    Name:        helloworld
    Version:     0.1
    Synopsis:    Hello world with OCaml and OASIS
    Authors:     Sylvain Le Gall
    License:     LGPL
 
    Executable helloworld
      Path:   .
      MainIs: main.ml

We generate the file setup.ml and everything needed by ocamlbuild:

    $> oasis setup
    I: File setup.ml doesn't exist, creating it.
    I: File _tags doesn't exist, creating it.
    I: File myocamlbuild.ml doesn't exist, creating it.

We can now go into the configure/build/install steps:

    $> ocaml setup.ml -configure 

    I: Running command '/usr/bin/ocamlc.opt -config > /tmp/oasis-caeb68.txt'
    I: Running command '/usr/bin/ocamlfind query -format %v findlib > /tmp/oasis-780a0b.txt'

    Configuration: 

    Package version: ............................... 0.1
    Package name: .................................. helloworld
    suffix_program: ................................ 
    is_native: ..................................... true

    [...]

    ocamlopt: ...................................... /usr/bin/ocamlopt.opt
    ocamlc: ........................................ /usr/bin/ocamlc.opt
    ocamlfind: ..................................... /usr/bin/ocamlfind

    $> ocaml setup.ml -build

    [ocamlbuild running]

    $> ocaml setup.ml -install

    I: Copying file '_build/helloworld' to '/usr/local/bin/helloworld
    I: Running command 'cp _build/helloworld /usr/local/bin/helloworld

# Further

We have limited our example to a very simple case. To create a library you just
have to follow the same path, except that you will create a library section.

If you want to use external libraries, you can use the `BuildDepends` fields 
of the `Library` and `Executable` sections. The configure script will check
their presence and ocamlbuild will automatically tag your sources to use this
external library. This field should refer to a findlib package (OASIS don't 
support non-findlib OCaml library). You can use a library created inside
the project as well.
