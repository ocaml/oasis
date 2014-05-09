#include "header.h"
                     CAMLprim value oasis_c_build_test_foo(value x) {
                       CAMLparam1(x);
                       CAMLreturn(Val_int(42));
                     }
