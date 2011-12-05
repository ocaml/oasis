
(** Handle 'pager' command
  *)

open BaseMessage
open OASISGettext
open OASISUtils

let pager_cmd =
  try 
    Some 
      (try 
         Sys.getenv "OASIS_PAGER"
       with Not_found ->
         begin
           try
             Sys.getenv "PAGER"
           with Not_found ->
             FileUtil.which "pager"
         end)
  with Not_found ->
    None

let open_out () = 
  let buf = 
    Buffer.create 13
  in
  let fmt = 
    Format.formatter_of_buffer buf
  in
    (buf, fmt),
    fmt


let close_out (buf, fmt) = 
  let () = 
    Format.pp_print_flush fmt ()
  in
    match pager_cmd with 
      | Some cmd ->
          begin
            let proc = 
              Unix.open_process_out cmd
            in
            let () = 
              Buffer.output_buffer proc buf
            in
              match Unix.close_process_out proc with 
                | Unix.WEXITED 0 ->
                    ()
                | Unix.WEXITED n 
                | Unix.WSIGNALED n 
                | Unix.WSTOPPED n ->
                    failwithf
                      (f_ "Command %S exited with error code %d")
                      cmd n
          end

      | None ->
          begin
            warning "%s" "Environment variable OASIS_PAGER not set.";
            Buffer.output_buffer stdout buf
          end
