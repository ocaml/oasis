
(** Running commands 
    @author Sylvain Le Gall
  *)

(** Run a command 
  *)
let run cmd args =
  let cmdline =
    String.concat " " (cmd :: args)
  in
    BaseMessage.info 
      (Printf.sprintf "Running command '%s'" cmdline);
    match Sys.command cmdline with 
      | 0 ->
          ()
      | i ->
          failwith 
            (Printf.sprintf 
               "Command '%s' terminated with error code %d"
               cmdline i)
;;

(** Run a command and returns its output
  *)
let run_read_output cmd args =
  let cmdline =
    String.concat " " (cmd :: args)
  in
  let chn =
    BaseMessage.info 
      (Printf.sprintf "Running command '%s'" cmdline);
    Unix.open_process_in cmdline
  in
  let routput =
    ref []
  in
    (
      try
        while true do 
          routput := (input_line chn) :: !routput
        done
      with End_of_file ->
        ()
    );
    (
      match Unix.close_process_in chn with
        | Unix.WEXITED 0 -> 
            ()
        | Unix.WEXITED i ->
            failwith 
              (Printf.sprintf
                 "Command '%s' terminated with error code %d"
                 cmdline i)
        | Unix.WSIGNALED i ->
            failwith
              (Printf.sprintf
                 "Command '%s' has been killed by signal %d"
                 cmdline i)
        | Unix.WSTOPPED i  -> 
            failwith
              (Printf.sprintf
                 "Command '%s' has been stopped by signal %d"
                 cmdline i)
    );
    List.rev !routput
;;

