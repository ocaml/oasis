
#include <caml/memory.h>

CAMLprim value caml_reident_native (value va)
{
  CAMLparam1(va);
  CAMLreturn(va);
}
