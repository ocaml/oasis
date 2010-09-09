
(** Test OASISValues defined fields
    @author Sylvain Le Gall
  *)

open TestCommon
open OASISValues
open OUnit

let tests ctxt =
  "Values" >:::
  (List.map 
     (fun (v, txt) ->
        TestCase
          (fun () ->
             let _a =
               v.parse 
                 ~ctxt:ctxt.oasis_ctxt
                 txt
             in
               ())))
    [url, "https://oasis.forge.ocamlcore.org";
     url, "svn://scm.ocamlcore.org/svnroot/cryptokit/tags";
     url, "svn+ssh://scm.ocamlcore.org/svnroot/cryptokit/tags"]


