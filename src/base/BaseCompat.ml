
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
  let adapt_setup_t setup_t = setup_t
end


module Compat_0_3 =
struct
  let adapt_setup_t setup_t = setup_t
end
