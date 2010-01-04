
(** Check an OASIS package
  *)

open CommonGettext;;
open PropList;;

let check_schema schm data =
  let msgfld =
    Schema.fold
      (fun acc fld plugin hlp ->
         if plugin = None then
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
           end
         else
           acc)

      []
      schm
  in
    if msgfld <> [] then
      failwith 
        (Printf.sprintf
           (f_ "Missing field: %s")
           (String.concat (s_ ", ") msgfld))
;;
