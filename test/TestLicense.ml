(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Test for OASISLicense
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon
open OASISValues
open OASISTypes
open OASISLicense

let tests =
  let ver =
    OASISVersion.version_of_string
  in
  let mk ?(v=NoVersion) ?(e=[]) lic = 
    Some
      (DEP5License
         {
           license    = lic;
           version    = v;
           exceptions = e;
         })
  in

    "License" >:::
    (List.map
       (fun (txt, res) ->
          txt >::
          (fun () ->
             match res with
               | Some exp ->
                   assert_equal 
                     ~printer:to_string
                     exp
                     (value.parse ~ctxt:!oasis_ctxt txt)
               | None ->
                   try 
                     ignore (value.parse ~ctxt:!oasis_ctxt txt);
                     assert_failure 
                       (Printf.sprintf
                          "License '%s' is not valid but parse without problem"
                          txt)
                   with e ->
                     ()))

       [
         "BSD4", 
         mk bsd4;

         "BSD4-1.0+", 
         mk ~v:(VersionOrLater (ver "1.0")) bsd4;

         "BSD3", 
         mk bsd3;

         "GPL",  
         mk gpl;

         "GPL-2", 
         mk ~v:(Version (ver "2")) gpl;

         "GPL-2+", 
         mk ~v:(VersionOrLater (ver "2")) gpl;

         "LGPL-2.1 with OCaml linking exception",
         mk ~v:(Version (ver "2.1")) ~e:[ocaml_linking_exception] lgpl;

         "http://some.stuff.com/license",
         Some (OtherLicense "http://some.stuff.com/license");

         "CeCILL",
         mk cecill;

         "CeCILL-B",
         mk cecillb;

         "CeCILL-C",
         mk cecillc;

         "LGPL-2.1 with OCaml drinking exception",
         None;

         "CeCILLB2000",
         None;

         "CMU/MIT",
         None;
       ]

    )
