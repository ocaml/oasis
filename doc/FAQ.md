Using OASIS
===========

How to create a string flag?<a name="string-flag"></a>
----------------------------

Flag in `_oasis` are only boolean. As of OASIS 0.4.7, there is no way to define
a string flag. A string flag, can be useful when you want e.g.
`CCOpt: -D $(STR)`.

The reason why this has not been done are:
* boolean flags are easy to switch on and off to test various branches, which
  can allow auto exploration for the package.
* string flags would require a way to validate them, make sure that quoting
  is correct when they are used in substitution.
* string flags are easily implementable directly in `setup.ml`.

The quick solution to have a string flag `FOO` is to add the following code to
`setup.ml`:

    (* OASIS_STOP *)
    let _ = BaseEnv.var_define "FOO" (fun _ -> "123");;
    let () = setup ();;

This can be used like this:

    Executable "opam-build-revdeps"
      Path: src/bin/opam-build-revdeps
      MainIs: OPAMBuildRevdeps.ml
      CompiledObject: best
      CCOpt:-D $(FOO)


The value of the flag `FOO` can be set through command line and environment
variable.

