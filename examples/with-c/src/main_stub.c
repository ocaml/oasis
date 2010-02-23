
#include <caml/memory.h>

#ifndef OPTION_PASSED
#error "CCOpt doesn't work"
#endif

CAMLprim value caml_reident (value va)
{
  CAMLparam1(va);
  CAMLreturn(va);
}
