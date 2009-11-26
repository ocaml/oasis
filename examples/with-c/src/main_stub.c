
#include <caml/memory.h>

CAMLprim value caml_reident (value va)
{
  CAMLparam1(va);
  CAMLreturn(va);
}
