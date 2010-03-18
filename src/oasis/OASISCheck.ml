
(** Check an OASIS package
  *)

open OASISGettext
open OASISSchema
open OASISUtils
open PropList

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
      failwithf2
        (f_ "Missing field in %s: %s")
        where
        (String.concat (s_ ", ") msgfld)
