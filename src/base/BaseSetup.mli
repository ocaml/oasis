(** Entry points for 'setup.ml'
    @author Sylvain Le Gall
  *)

open OASISTypes

type std_args_fun = 
    package -> arg array -> unit

type ('a, 'b) section_args_fun = 
    name * (package -> (common_section * 'a) -> arg array -> 'b)

type t =
    {
      configure:       std_args_fun;
      build:           std_args_fun;
      doc:             ((doc, unit)  section_args_fun) list;
      test:            ((test, float) section_args_fun) list;
      install:         std_args_fun;
      uninstall:       std_args_fun;
      clean:           std_args_fun list;
      clean_doc:       (doc, unit) section_args_fun list;
      clean_test:      (test, unit) section_args_fun list;
      distclean:       std_args_fun list;
      distclean_doc:   (doc, unit) section_args_fun list;
      distclean_test:  (test, unit) section_args_fun list;
      package:         package;
      version:         string; (* OASIS version that generates this structure *)
    } 

(** Run the configure step.
  *)
val configure : t -> arg array -> unit

(** Run the build step.
  *)
val build : t -> arg array -> unit

(** Run the doc step: build all documents.
  *)
val doc : t -> arg array -> unit

(** Run the test step: run all tests.
  *)
val test : t -> arg array -> unit

(** Run the install step.
  *)
val install : t -> arg array -> unit

(** Run the uninstall step.
  *)
val uninstall : t -> arg array -> unit

(** Run the clean step.
  *)
val clean : t -> arg array -> unit

(** Run the distclean step.
  *)
val distclean : t -> arg array -> unit

(** Run the reinstall step: deinstall and install.
  *)
val reinstall : t -> arg array -> unit

(** Run all steps: configure, build, doc, test and install.
  *)
val all : t -> arg array -> unit

(** Display OASIS version used to generate this setup.ml
  *)
val version: t -> arg array -> unit

(** The first function called when running 'setup.ml'.
  *)
val setup : t -> unit

(** Default filename for 'setup.ml'. {b Not exported}
  *)
val default_filename: host_filename

(** Get template 'setup.ml' file out of the plugin context. 
    {b Not exported}.
  *)
val find : OASISPlugin.context_act -> OASISFileTemplate.template

(** Create [t] and plugin context from an OASIS package.
    {b Not exported}.
  *)
val of_package : package -> OASISPlugin.context_act * t
