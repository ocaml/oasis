#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <stringprep.h>


value caml_stringprep_xmpp_nodeprep( value v_in )
{
  CAMLparam1( v_in );
  char *in;
  int maxlen;
  int res;

  in = String_val(v_in);
  maxlen = caml_string_length(v_in);
  res = stringprep_xmpp_nodeprep(in, maxlen);

  CAMLreturn( Val_int(res) );
}
