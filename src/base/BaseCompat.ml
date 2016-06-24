
(** Compatibility layer to provide a stable API inside setup.ml.
    This layer allows OASIS to change in between minor versions
    (e.g. 0.4.6 -> 0.4.7) but still provides a stable API inside setup.ml. This
    enables to write functions that manipulate setup_t inside setup.ml. See
    deps.ml for an example.

    The module opened by default will depend on the version of the _oasis. E.g.
    if we have "OASISFormat: 0.3", the module Compat_0_3 will be opened and
    the function Compat_0_3 will be called. If setup.ml is generated with the
    -no-compat, no module will be opened.

    @author Sylvain Le Gall
  *)

module Compat_0_4 =
struct
  let rctxt = ref !BaseContext.default

  module BaseSetup =
  struct
    module Original = BaseSetup

    open OASISTypes

    type std_args_fun = package -> string array -> unit
    type ('a, 'b) section_args_fun =
      name * (package -> (common_section * 'a) -> string array -> 'b)
    type t =
      {
        configure:        std_args_fun;
        build:            std_args_fun;
        doc:              ((doc, unit)  section_args_fun) list;
        test:             ((test, float) section_args_fun) list;
        install:          std_args_fun;
        uninstall:        std_args_fun;
        clean:            std_args_fun list;
        clean_doc:        (doc, unit) section_args_fun list;
        clean_test:       (test, unit) section_args_fun list;
        distclean:        std_args_fun list;
        distclean_doc:    (doc, unit) section_args_fun list;
        distclean_test:   (test, unit) section_args_fun list;
        package:          package;
        oasis_fn:         string option;
        oasis_version:    string;
        oasis_digest:     Digest.t option;
        oasis_exec:       string option;
        oasis_setup_args: string list;
        setup_update:     bool;
      }

    let setup t =
      let mk_std_args_fun f =
        fun ~ctxt pkg args -> rctxt := ctxt; f pkg args
      in
      let mk_section_args_fun l =
        List.map
          (fun (nm, f) ->
             nm,
             (fun ~ctxt pkg sct args ->
                rctxt := ctxt;
                f pkg sct args))
          l
      in
      let t' =
        {
          Original.
          configure =        mk_std_args_fun t.configure;
          build =            mk_std_args_fun t.build;
          doc =              mk_section_args_fun t.doc;
          test =             mk_section_args_fun t.test;
          install =          mk_std_args_fun t.install;
          uninstall =        mk_std_args_fun t.uninstall;
          clean =            List.map mk_std_args_fun t.clean;
          clean_doc =        mk_section_args_fun t.clean_doc;
          clean_test =       mk_section_args_fun t.clean_test;
          distclean =        List.map mk_std_args_fun t.distclean;
          distclean_doc =    mk_section_args_fun t.distclean_doc;
          distclean_test =   mk_section_args_fun t.distclean_test;

          package =          t.package;
          oasis_fn =         t.oasis_fn;
          oasis_version =    t.oasis_version;
          oasis_digest =     t.oasis_digest;
          oasis_exec =       t.oasis_exec;
          oasis_setup_args = t.oasis_setup_args;
          setup_update =     t.setup_update;
        }
      in
      Original.setup t'

  end

  let adapt_setup_t setup_t =
    let module O = BaseSetup.Original in
    let mk_std_args_fun f = fun pkg args -> f ~ctxt:!rctxt pkg args in
    let mk_section_args_fun l =
      List.map
        (fun (nm, f) -> nm, (fun pkg sct args -> f ~ctxt:!rctxt pkg sct args))
        l
    in
    {
      BaseSetup.
      configure =        mk_std_args_fun setup_t.O.configure;
      build =            mk_std_args_fun setup_t.O.build;
      doc =              mk_section_args_fun setup_t.O.doc;
      test =             mk_section_args_fun setup_t.O.test;
      install =          mk_std_args_fun setup_t.O.install;
      uninstall =        mk_std_args_fun setup_t.O.uninstall;
      clean =            List.map mk_std_args_fun setup_t.O.clean;
      clean_doc =        mk_section_args_fun setup_t.O.clean_doc;
      clean_test =       mk_section_args_fun setup_t.O.clean_test;
      distclean =        List.map mk_std_args_fun setup_t.O.distclean;
      distclean_doc =    mk_section_args_fun setup_t.O.distclean_doc;
      distclean_test =   mk_section_args_fun setup_t.O.distclean_test;

      package =          setup_t.O.package;
      oasis_fn =         setup_t.O.oasis_fn;
      oasis_version =    setup_t.O.oasis_version;
      oasis_digest =     setup_t.O.oasis_digest;
      oasis_exec =       setup_t.O.oasis_exec;
      oasis_setup_args = setup_t.O.oasis_setup_args;
      setup_update =     setup_t.O.setup_update;
    }
end


module Compat_0_3 =
struct
  include Compat_0_4
end
