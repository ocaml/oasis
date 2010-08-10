
(** Test OASISValues defined fields
    @author Sylvain Le Gall
  *)

open TestCommon
open OASISValues
open OUnit

let tests ctxt =
  "Values" >::
  (fun () ->
     let v =
       url
     in
     let _a =
       v.parse 
         ~ctxt:OASISContext.quiet 
         "https://oasis.forge.ocamlcore.org"
     in
       ())
         


