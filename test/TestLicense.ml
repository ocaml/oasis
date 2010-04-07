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
open OASISLicense
open OASISTypes

let tests ctxt =
  let ver =
    OASISVersion.version_of_string
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
                     (parse txt)
               | None ->
                   try 
                     ignore (parse txt);
                     assert_failure 
                       (Printf.sprintf
                          "License '%s' is not valid but parse without problem"
                          txt)
                   with e ->
                     ()))

       [
         "BSD4", 
         Some BSD4;

         "BSD4-1.0+", 
         None;

         "BSD3", 
         Some BSD3;

         "GPL",  
         Some GPL;

         "GPL-2", 
         Some (LicenseWithVersion(GPL, ver "2"));

         "GPL-2+", 
         Some (LicenseWithLaterVersion(GPL, ver "2"));

         "LGPL-2.1 with OCaml linking exception",
         Some (LicenseWithException
                 (LicenseWithVersion (LGPL, ver "2.1"),
                  OCamlLinkingException));

         "http://some.stuff.com/license",
         Some (OtherLicense "http://some.stuff.com/license");
       ]

    )
