
(** Common utilities for testing
    @author Sylvain Le Gall
  *)

open OUnit;;

module MapString = Map.Make(String);;
module SetString = Set.Make(String);;

type context =
    {
      dbug:         bool;
      long:         bool;
      has_ocamlopt: bool;
      oasis:        string;
      oasis_args:   string list;
    }
;;

let in_data fn =
  Filename.concat "data" fn
;;

(* Assert checking that command run well *)
let assert_command ?(exit_code=0) ?(extra_env=[]) ctxt cmd args  =
  let cmdline =
    String.concat " " 
      (
        (if extra_env <> [] then
           "env" :: (List.map (fun (nm, vl) -> nm^"="^vl) extra_env)
         else
           [])
        @
        (cmd :: args)
      )
  in
  let fn, chn_out =
    Filename.open_temp_file "oasis-" ".log"
  in
  let fd =
    Unix.descr_of_out_channel chn_out
  in
  let clean_fn () =
    FileUtil.rm [fn]
  in
  let () = 
    at_exit clean_fn
  in
  let env = 
    let extra_env_map = 
      (* Build a map of asked replacement *)
      List.fold_left
        (fun mp (k,v) ->
           MapString.add k v mp)
        MapString.empty
        extra_env
    in
    let split_variable e = 
      (* Extract variable from string of the form "k=v" *)
      try 
        let idx = 
          String.index e '='
        in
          String.sub e 0 idx,
          String.sub e (idx + 1) ((String.length e) - idx - 1)
      with Not_found ->
        e, ""
    in
    let rev_lst, extra_env_map =
      (* Go through current enviromnent and replace "k=v" 
       * when k is in extra_env, remove this key at the
       * same time to avoid duplication when re-creating
       * the environment, at the end 
       *)
      List.fold_left
        (fun (acc,mp) e ->
           let k,v =
             split_variable e
           in
           let v, mp =
             try 
               MapString.find k mp,
               MapString.remove k mp
             with Not_found ->
               v, mp
           in
             ((k, v) :: acc), mp)
        ([], extra_env_map)
        (Array.to_list (Unix.environment ()))
    in
    let rev_lst =
      (* Add key from extra_env that has not been replaced *)
      List.fold_left
        (fun acc ((k, _) as e) ->
           if MapString.mem k extra_env_map then
             e :: acc
           else
             acc)
        rev_lst
        extra_env
    in
      Array.of_list (List.rev_map (fun (k,v) -> k^"="^v) rev_lst)
  in

  let pid =
    let res = 
      if ctxt.dbug then
        prerr_endline ("Running "^cmdline); 
      Unix.create_process_env 
        cmd 
        (Array.of_list (cmd :: args))
        env
        Unix.stdin
        fd
        fd
    in
      close_out chn_out;
      res
  in
  let dump_stdout_stderr () = 
    let buff =
      Buffer.create 13
    in
    let chn_in = 
      open_in_bin fn
    in
      Buffer.add_channel buff chn_in (in_channel_length chn_in);
      Buffer.output_buffer stderr buff;
      flush stderr;
      close_in chn_in
  in
  let err_stdout_stderr () = 
    dump_stdout_stderr ();
    Printf.eprintf "Error running command '%s'\n%!" cmdline
  in
    begin
      match Unix.waitpid [] pid with
        | _, Unix.WEXITED i ->
            if i <> exit_code then
              err_stdout_stderr ()
            else if ctxt.dbug then
              begin
              dump_stdout_stderr ();
              end;
            assert_equal
              ~msg:"exit code"
              ~printer:string_of_int
              exit_code
              i;
        | _, Unix.WSIGNALED i ->
            err_stdout_stderr ();
            failwith 
              (Printf.sprintf
                 "Process '%s' has been killed by signal %d"
                 cmdline
                 i)
        | _, Unix.WSTOPPED i ->
            err_stdout_stderr ();
            failwith
              (Printf.sprintf
                 "Process '%s' has been stopped by signal %d"
                 cmdline
                 i)
    end;
    clean_fn ()
