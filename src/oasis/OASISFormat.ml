
open OASISSchema
open OASISTypes
open Format
open FormatExt

(** Pretty printing of OASIS files
  *)

let pp_print_fields fmt (schm, data) = 
  let fake_data =
    PropList.Data.create ()
  in
  let key_value =
    List.rev
      (PropList.Schema.fold 
         (fun acc key extra _ ->
            try 
              let str =
                PropList.Schema.get 
                  schm
                  data
                  key
              in
              let is_default =
                try 
                  let default =
                    PropList.Schema.get 
                      schm
                      fake_data
                      key 
                  in
                    str = default
                with 
                  | OASISValues.Not_printable 
                  | PropList.Not_set _ ->
                      (* Unable to compare so this is not default *)
                      false
              in
                if not is_default then
                  (key, str) :: acc
                else
                  acc
            with 
              | OASISValues.Not_printable ->
                  acc 
              | PropList.Not_set _ when extra.plugin <> None ->
                  acc)
         []
         schm)
  in
    
  let max_key_length =
    (* ":" *)
    1 
    +
    (* Maximum length of a key *)
    (List.fold_left
       max
       0

       (* Only consider length of key *)
       (List.rev_map
          fst

          (* Remove key/value that exceed line length *)
          (List.filter 
             (fun (k, v) -> k + v < pp_get_margin fmt ())
             
             (* Consider only length of key/value *)
             (List.rev_map
                (fun (k, v) -> String.length k, String.length v)
                key_value))))
  in

    pp_open_vbox fmt 0;
    List.iter
      (fun (k, v) ->
         pp_open_box fmt 2;
         pp_print_string fmt k;
         pp_print_string fmt ":";
         pp_print_break fmt (max 0 (max_key_length - String.length k)) 0;
         pp_print_string_spaced fmt v;
         pp_close_box fmt ();
         pp_print_cut fmt ())
      key_value;
    pp_close_box fmt ()


let pp_print_package_proplist fmt (pkg_data, section_data) =

  pp_open_vbox fmt 0;

  pp_print_fields fmt (OASISPackage.schema, pkg_data);
  pp_print_cut fmt ();

  List.iter 
    (fun sct ->
       let schm = 
         (* TODO: the sections list should embed this schema *)
         match sct with 
           | Library _ ->
               OASISLibrary.schema
           | Executable _ -> 
               OASISExecutable.schema
           | SrcRepo _ ->
               OASISSourceRepository.schema
           | Test _ ->
               OASISTest.schema
           | Flag _ ->
               OASISFlag.schema
           | Doc _ ->
               OASISDocument.schema
       in

       let {cs_name = nm; cs_data = data} = 
         OASISSection.section_common sct
       in 

       let pp_id_or_string fmt str =
         (* A string is an id if varname_of_string doesn't change it *)
         if str = (OASISUtils.varname_of_string str) then 
           fprintf fmt "%s" str
         else 
           fprintf fmt "%S" str
       in
         fprintf fmt "@[<v 2>%s %a@,%a@]@,"
           schm.PropList.Schema.name
           pp_id_or_string nm
           pp_print_fields (schm, data))
    section_data;

  pp_close_box fmt ()

