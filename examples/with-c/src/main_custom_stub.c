
#include <caml/memory.h>

CAMLprim value caml_reident_custom (value va)
{
  CAMLparam1(va);
  CAMLreturn(va);
}
