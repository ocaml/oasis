

(** Test quickstart subcommand
    @author Sylvain Le Gall
  *)

open TestCommon
open Expect
open OUnit
open OASISTypes

module MapString = Map.Make(String)

let tests ctxt = 
  let run_quickstart args qa = 
    let args = 
      ctxt.oasis_args @ ["-quiet"; "quickstart"; "-machine"] @ args
    in
    let () = 
      if ctxt.dbug then
        Printf.eprintf 
          "Quickstart command line: %s\n%!" 
          (String.concat " " (ctxt.oasis :: args))
    in
    let _, exit_code = 
      try 
        with_spawn
          ~verbose:ctxt.dbug
          ~timeout:(Some 0.1)
          ctxt.oasis 
          (Array.of_list args)
          (fun t () ->
             let rec continue = 
               function
                 | [] ->
                     begin
                       ()
                     end

                 | qa -> 
                     begin
                       let expectations = 
                         let rec expectations' prev next = 
                           match next with 
                             | (q, a) :: tl ->
                                 (* next QA if we chose this answer *)
                                 let qa' = 
                                   List.rev_append prev tl
                                 in
                                 let full_lowercase_q = 
                                   match q with 
                                     | "create_section" ->
                                         "create a section?"
                                     | "end" ->
                                         "package definition is complete"
                                     | q ->
                                         Printf.sprintf 
                                           "value for field '%s'? "
                                           q
                                 in
                                   (* TODO: expect + prefix *)
                                   (ExpectFun 
                                      (fun str ->
                                         (String.length str >= 
                                            String.length full_lowercase_q)
                                         &&
                                         (String.lowercase 
                                            (String.sub str 
                                               0 
                                               (String.length full_lowercase_q))
                                            =  
                                            full_lowercase_q)), 
                                    Some (a, qa'))
                                   ::
                                   (ExpectExact ("???"^q^" "), Some (a, qa'))
                                   ::
                                   (expectations' ((q, a) :: prev) tl)
                             | [] ->
                                 []
                         in
                           expectations' [] qa
                       in
                       let exp_q = 
                         expect t expectations None
                       in

                         match exp_q with 
                           | Some (a, qa) -> 
                               send t (a^"\n");
                               continue qa
                           | None -> 
                               let assert_msg = 
                                 Printf.sprintf "expecting questions: %s"
                                   (String.concat ", "
                                      (List.map 
                                         (fun (q, _) -> Printf.sprintf "%S" q)
                                         qa))
                               in
                                 assert_failure assert_msg
                     end
             in
              continue qa;
              assert_bool
                "wait for eof"
                (expect t [ExpectEof, true] false);
          ) ()

      with e ->
        Printexc.print_backtrace stderr;
        raise e
    in
      assert_equal 
        ~msg:"exit code"
        ~printer:(function 
                    | Unix.WEXITED i 
                    | Unix.WSIGNALED i 
                    | Unix.WSTOPPED i -> 
                        string_of_int i)
        (Unix.WEXITED 0)
        exit_code
  in
  let test_of_vector (nm, args, qa) = 
    nm >::
    bracket 
      (fun () ->
         let pwd = FileUtil.pwd () in
         let tmp = temp_dir () in
           Sys.chdir tmp;
           pwd, tmp)
      (fun _ ->
         run_quickstart args qa;
         if ctxt.dbug then 
           begin
             let chn = open_in "_oasis" in
             let () = 
               try 
                 while true do 
                   prerr_endline (input_line chn)
                 done
               with End_of_file ->
                 ()
             in
               close_in chn
           end;
         assert_oasis_cli ctxt ["check"];
         begin
           try 
             assert_oasis_cli ctxt ["setup"];
           with e ->
             failwith "'OASIS setup' failed but 'OASIS check' succeed"
         end;
         let pkg = 
           OASISParse.from_file ~ctxt:ctxt.oasis_ctxt "_oasis"
         in
         let () = 
           assert_equal 
             ~msg:"field name"
             ~printer:(fun s -> s)
             "test" pkg.name
         in

         let sct = 
           try 
             OASISSection.section_find 
               (OASISSection.KExecutable,
                "test")
               pkg.sections
           with Not_found ->
              failwith "Cannot find executable section 'test'"
         in
           match sct with 
             | Executable (cs, bs, exec) ->
                 assert_equal 
                   ~msg:"mainis of test"
                   ~printer:(fun s -> s)
                   "test.ml" exec.exec_main_is 
             | _ ->
                 assert false
      )
      (fun (pwd, tmp) ->
         Sys.chdir pwd;
         FileUtil.rm ~recurse:true [tmp])
  in
    "Quickstart" >:::
    (List.map test_of_vector 
       [
         "simple",
         [],
         [
           "name",           "test";
           "version",        "0.0.1";
           "synopsis",       "test";
           "authors",        "me";
           "license",        "GPL-2+";
           "create_section", "e";
           "name",           "test";
           "path",           "./";
           "mainis",         "test.ml";
           "create_section", "n";
           "end",            "w";
         ];

(*
         "custom",
         ["-level"; "expert"],
         [
           "name", "test";
           "version", "0.1";
           "synopsis", "test";
           "description", "test";
           "licensefile", "";
           "authors", "me";
           "copyrights", "(C) 2010 Me";
           "maintainers", "";
           "license", "GPL-3";
           "ocamlversion", "";
           "findlibversion", "";
           "conftype", "custom";
           "preconfcommand", "";
           "postconfcommand", "";
           "buildtype", "custom";
           "prebuildcommand", "";
           "postbuildcommand", "";
           "installtype", "";
           "preinstallcommand", "";
           "postinstallcommand", "";
           "preuninstallcommand", "";
           "postuninstallcommand", "";
           "precleancommand", "";
           "postcleancommand", "";
           "predistcleancommand", "";
           "postdistcleancommand", "";
           "homepage", "";
           "categories", "";
           "filesab", "";
           "plugins", "";
           "builddepends", "";
           "buildtools", "";
           "create_section", "n";
           "end", "w";
         ]
 *)
       ])

