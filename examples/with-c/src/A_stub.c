
#include <caml/memory.h>

#ifndef OPTION_PASSED
#error "CCOpt doesn't work"
#endif

CAMLprim value caml_ident (value vs)
{
  CAMLparam1(vs);
  CAMLreturn(vs);
};
