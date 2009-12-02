
(** Maintain a DB of what has been done
    @author Sylvain Le Gall
  *)

open BaseUtils;;

let default_filename =
  Filename.concat 
    (Filename.dirname BaseEnvRO.default_filename)
    "setup.log"
;;

let load () = 
  if Sys.file_exists default_filename then
    (
      let chn = 
        open_in default_filename
      in
      let rec read_aux acc =
        try 
          (
            read_aux 
              (Scanf.fscanf chn "%S %S\n" 
                 (fun e d ->  (e, d) :: acc))
          )
        with End_of_file ->
          (
            close_in chn;
            List.rev acc
          )
      in
        read_aux []
    )
  else
    (
      []
    )
;;

let register event data =
  let chn_out =
    open_out_gen [Open_append; Open_creat; Open_text] 0o644 default_filename
  in
    Printf.fprintf chn_out "%S %S\n" event data;
    close_out chn_out
;;

let unregister event data =
  let lst = 
    load ()
  in
  let chn_out =
    open_out default_filename
  in
    List.iter 
      (fun (e, d) ->
         if e <> event || d <> data then
           Printf.fprintf chn_out "%S %S\n" event data)
      lst;
    close_out chn_out
;;

let filter events =
  let st_events =
    List.fold_left
      (fun st e -> 
         SetString.add e st)
      SetString.empty
      events
  in
    List.filter 
      (fun (e, _) -> SetString.mem e st_events)
      (load ())
;;
