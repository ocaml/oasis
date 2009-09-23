
(** Run full OCamlAutobuild use case
    @author Sylvain Le Gall
  *)

open TestCommon;;
open OUnit;;

let tests ctxt =
  let curdir = 
    Sys.getcwd ()
  in

  let assert_command ?(exit_code=0) cmd args =
    let cmdline =
      String.concat " " (cmd :: args)
    in
    let fn, chn_out =
      Filename.open_temp_file "ocaml-autobuild-" ".log"
    in
    let chn_in =
      open_in fn
    in
    let () = 
      Sys.remove fn
    in
    let fd =
      Unix.descr_of_out_channel chn_out
    in
    let pid =
      Unix.create_process 
        cmd 
        (Array.of_list (cmd :: args))
        Unix.stdin
        fd
        fd;
    in
    let dump_stdout_stderr () = 
      let buff =
        Buffer.create 13
      in
        close_out chn_out;
        Buffer.add_channel buff chn_in (in_channel_length chn_in);
        close_in chn_in;
        Printf.eprintf "Error running command '%s'\n%!" cmdline;
        Buffer.output_buffer stderr buff;
        flush stderr
    in
      match Unix.waitpid [] pid with
        | _, Unix.WEXITED i ->
            if i <> exit_code then
              dump_stdout_stderr ();
            assert_equal
              ~msg:"exit code"
              ~printer:string_of_int
              exit_code
              i;
            close_in chn_in;
            close_out chn_out
        | _, Unix.WSIGNALED i ->
            dump_stdout_stderr ();
            failwith 
              (Printf.sprintf
                 "Process '%s' has been killed by signal %d"
                 cmdline
                 i)
        | _, Unix.WSTOPPED i ->
            dump_stdout_stderr ();
            failwith
              (Printf.sprintf
                 "Process '%s' has been stopped by signal %d"
                 cmdline
                 i)
  in

  let test_of_vector srcdir =
    srcdir >::
    (fun () ->
       Sys.chdir curdir;
       assert_command "../_build/src/OCamlAutobuild.byte" ["-C"; srcdir];
       Sys.chdir srcdir;

       if Sys.file_exists "setup.data" then
         Sys.remove "setup.data";

       assert_command "ocaml" ["setup.ml"; "-configure"];
       assert_command "ocaml" ["setup.ml"; "-build"];
       (* TODO: reactivate 
        * assert_command "ocaml" ["setup.ml"; "-install"];
        *)
       assert_command "ocaml" ["setup.ml"; "-clean"];
       assert_command "ocaml" ["setup.ml"; "-distclean"];

       Sys.chdir curdir)
  in

    "TestFull" >:::
    (List.map test_of_vector
       [
         "../examples/flags";
         "../examples/simplelib";
         "../examples/findlib";
         "../examples/custom";
       ])
;;
