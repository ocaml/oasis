
(** Build documentation
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes
open OASISGettext

let doc lst pkg extra_args =

  let one_doc (doc_plugin, cs, doc) = 
    if var_choose doc.doc_build then
      begin
        OASISMessage.info (f_ "Building documentation '%s'") cs.cs_name;
        doc_plugin pkg (cs, doc) extra_args
      end
  in
    List.iter 
      one_doc
      lst

(* END EXPORT *)
