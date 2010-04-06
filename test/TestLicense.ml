
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
