
#include <caml/memory.h>

CAMLprim value caml_ident (value vs)
{
  CAMLparam1(vs);
  CAMLreturn(vs);
};
