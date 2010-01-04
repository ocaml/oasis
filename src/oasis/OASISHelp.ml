
(** Display help for OASIS
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open CommonGettext;;
open Format;;

let pp_string_spaced fmt str =
  String.iter
    (function
       | ' ' -> Format.pp_print_space fmt ()
       | c -> Format.pp_print_char fmt c)
    str
;;

let pp_section ?plugin ?(section_txt="==") fmt schm = 
  let fields =
    PropList.Schema.fold
      (fun acc key plg help ->
         if plugin = plg then
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
               )
             in
               fprintf fmt 
                 (f_ " * @[%s: %a@]@\n") 
                 key
                 pp_string_spaced help
          )
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
