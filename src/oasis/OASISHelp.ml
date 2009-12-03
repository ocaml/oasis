
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
    List.fold_left 
      (fun acc key ->
         let descr = 
           try 
             Hashtbl.find schm.fields (String.lowercase key)
           with Not_found ->
             failwith 
               (Printf.sprintf
                  (f_ "Field %s not found")
                  key)
         in
           if descr.plugin = plugin then
             (key, descr) :: acc
           else
             acc)
      []
      schm.order
  in
    if fields <> [] then
      (
        fprintf fmt "@\n%s " section_txt;
        fprintf fmt (f_ "%s description") (String.capitalize schm.name);
        fprintf fmt " %s@\n@\n" section_txt;
        List.iter
          (fun (key, descr) ->
             let help = 
               descr.help ^
               (
                 try 
                   descr.get ();
                   ""
                 with (MissingField _) ->
                   s_ " (mandatory)"
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
