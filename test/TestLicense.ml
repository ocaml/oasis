(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


(** Test for OASISLicense
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open OASISValues
open OASISTypes
open OASISLicense


let tests =
  let ver =
    OASISVersion.version_of_string
  in
  let mk_base ?(v=NoVersion) ?e lic =
      (DEP5Unit
         {
           license  = lic;
           version  = v;
           excption = e;
         })
  in
  let mk ?v ?e lic =
    Some (DEP5License (mk_base ?v ?e lic))
  in

    "License" >:::
    (List.map
       (fun (txt, res) ->
          txt >::
          (fun test_ctxt ->
             match res with
               | Some exp ->
                   assert_equal
                     ~printer:(fun v -> Printf.sprintf "%S" (to_string v))
                     exp
                     (value.parse ~ctxt:oasis_ctxt txt)
               | None ->
                   try
                     ignore (value.parse ~ctxt:oasis_ctxt txt);
                     assert_failure
                       (Printf.sprintf
                          "License '%s' is not valid but parse without problem"
                          txt)
                   with e ->
                     ()))

       [
         "BSD-4-clause",
         mk bsd4;

         "BSD-4-clause-1.0+",
         None;
        
         (* TODO: Test also BSD3 because it was a deprecated synonym BSD-3-clause, same for 
            BSD2. *)

         "BSD-3-clause",
         mk bsd3;

         "BSD3",
         mk bsd3;

         "GPL",
         mk gpl;

         "GPL-2",
         mk ~v:(Version (ver "2")) gpl;

         "GPL-2+",
         mk ~v:(VersionOrLater (ver "2")) gpl;

         "LGPL-2.1 with OCaml linking exception",
         mk ~v:(Version (ver "2.1")) ~e:ocaml_linking_exception lgpl;

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

         "GPL-1+ or Artistic",
         Some
           (DEP5License
             (DEP5Or
                [
                  mk_base ~v:(VersionOrLater (ver "1")) gpl;
                  mk_base artistic;
                ]));

         "GPL-2+ and BSD-3-clause",
         Some
           (DEP5License
             (DEP5And
                [
                  mk_base ~v:(VersionOrLater (ver "2")) gpl;
                  mk_base bsd3;
                ]));

         "GPL-2+ or Artistic-2.0, and BSD-3-clause",
         Some
           (DEP5License
             (DEP5And
                [DEP5Or
                   [
                     mk_base ~v:(VersionOrLater (ver "2")) gpl;
                     mk_base ~v:(Version (ver "2.0")) artistic;
                   ];
                 mk_base bsd3]));
       ]

    )
