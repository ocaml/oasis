
(** Display help for OASIS
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open CommonGettext;;
open Format;;
open FormatExt;;

let pp_section ?plugin ?(section_txt="==") fmt schm = 
  let fields =
    PropList.Schema.fold
      (fun acc key extra help ->
         if plugin = extra.plugin then
           (key, help) :: acc
         else
           acc)
      []
      schm
  in
  let fake_data =
    PropList.Data.create ()
  in
    if fields <> [] then
      (
        fprintf fmt "@\n%s " section_txt;
        fprintf fmt (f_ "%s description") 
          (String.capitalize schm.PropList.Schema.name);
        fprintf fmt " %s@\n@\n" section_txt;
        List.iter
          (fun (key, help) ->
             let help = 
               (match help with
                  | Some h -> h ()
                  | None -> "<No help>")^
               (
                 try 
                   let _s : string = 
                     PropList.Schema.get schm fake_data key
                   in
                     ""
                 with 
                   | PropList.Not_set _ ->
                       s_ " (mandatory)"
                   | PropList.No_printer _ ->
                       ""
                   | OASISValues.Not_printable ->
                       ""
               )
             in
               fprintf fmt 
                 (f_ " * @[%s: %a@]@\n") 
                 key
                 pp_print_string_spaced help)
          fields
      )
;;

let pp_help ?plugin ?section_txt fmt () =
  List.iter 
    (pp_section ?plugin ?section_txt fmt) 
    [
      OASISPackage.schema;
      OASISFlag.schema;
      OASISLibrary.schema;
      OASISExecutable.schema;
    ]
;;
