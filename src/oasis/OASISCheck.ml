
(** Check an OASIS package
  *)

open CommonGettext
open PropList
open OASISSchema

let check_schema where schm data =
  let msgfld =
    Schema.fold
      (fun acc fld extra hlp ->
         match extra.plugin with 
           | None ->
               begin
                 try
                   let _ = 
                     Schema.get schm data fld
                   in 
                     acc
                 with 
                   | Not_set _ ->
                       fld :: acc 
                   | No_printer _ ->
                       acc
                   | OASISValues.Not_printable ->
                       acc
               end
           | Some _ ->
               begin
                 (* TODO: handle plugin checks *)
                 acc
               end)
      []
      schm
  in
    if msgfld <> [] then
      failwith 
        (Printf.sprintf
           (f_ "Missing field in %s: %s")
           where
           (String.concat (s_ ", ") msgfld))
