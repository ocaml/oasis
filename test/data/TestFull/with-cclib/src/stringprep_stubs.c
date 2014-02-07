/********************************************************************************/
/*  OASIS: architecture for building OCaml libraries and applications           */
/*                                                                              */
/*  Copyright (C) 2011-2013, Sylvain Le Gall                                    */
/*  Copyright (C) 2008-2011, OCamlCore SARL                                     */
/*                                                                              */
/*  This library is free software; you can redistribute it and/or modify it     */
/*  under the terms of the GNU Lesser General Public License as published by    */
/*  the Free Software Foundation; either version 2.1 of the License, or (at     */
/*  your option) any later version, with the OCaml static compilation           */
/*  exception.                                                                  */
/*                                                                              */
/*  This library is distributed in the hope that it will be useful, but         */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  */
/*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          */
/*  details.                                                                    */
/*                                                                              */
/*  You should have received a copy of the GNU Lesser General Public License    */
/*  along with this library; if not, write to the Free Software Foundation,     */
/*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               */
/********************************************************************************/

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
