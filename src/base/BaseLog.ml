
(** Maintain a DB of what has been done
    @author Sylvain Le Gall
  *)

open OASISUtils

(** Default file for registering log
  *)
let default_filename =
  Filename.concat 
    (Filename.dirname BaseEnv.default_filename)
    "setup.log"

(** Load the log file
  *)
let load () = 
  if Sys.file_exists default_filename then
    (
      let chn = 
        open_in default_filename
      in
      let rec read_aux acc =
        try 
          let line = 
            input_line chn
          in
            read_aux 
              (Scanf.sscanf line "%S %S" 
                 (fun e d ->  (e, d) :: acc))
        with End_of_file ->
          close_in chn;
          List.rev acc
      in
        read_aux []
    )
  else
    (
      []
    )

(** Add an event to the log file
  *)
let register event data =
  let chn_out =
    open_out_gen [Open_append; Open_creat; Open_text] 0o644 default_filename
  in
    Printf.fprintf chn_out "%S %S\n" event data;
    close_out chn_out

(** Remove an event from the log file
  *)
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
           Printf.fprintf chn_out "%S %S\n" e d)
      lst;
    close_out chn_out

(** Filter events of the log file
  *)
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

(** Check if an event exists in the log file 
  *)
let exists event data =
  List.exists
    (fun v -> (event, data) = v)
    (load ())
