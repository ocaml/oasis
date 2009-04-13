
(** Tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open Oasis;;

let tests ctxt =

  (* Convert environment to string *)
  let string_of_env env =
    String.concat "; "
      (
        (List.map (fun (nm, b)   -> Printf.sprintf "%s = %b" nm b) env.flags)
        @
        (List.map (fun (nm, str) -> Printf.sprintf "%s = '%s'" nm str) env.tests)
      )
  in

  (* Check flag equality *)
  let assert_flag nm lst =
    try
      let _ = 
        List.find 
          (fun (flg, _) -> nm = flg) 
          lst 
      in
        ()
    with Not_found ->
      assert_failure 
        (Printf.sprintf 
           "No flag '%s' defined"
           nm)
  in

  (* Check that at least one alternative doesn't raise an exception *)
  let assert_alternative msg lst e =
    let found_one =
      List.fold_left
        (fun r t ->
           if not r then
             (
               try
                 t e; true
               with _ ->
                 false
             )
           else
             r)
        false
        lst
    in
      if not found_one then
        assert_failure msg
  in

  let test_of_vector (fn, test) = 
    fn >::
    (fun () ->
       let fn =
         in_data fn
       in
       let ast =
         if ctxt.dbug then
           OasisTools.parse_file 
             ~fstream:OasisTools.stream_debugger 
             fn
         else
           OasisTools.parse_file
             fn
       in
       let (env, flags) =
         create fn ast  
       in
       let oasis =
         if ctxt.dbug then
           prerr_endline (string_of_env env);
         oasis (ast, env, flags)
       in
         test env flags oasis)
  in

    "OASIS" >:::
    (List.map test_of_vector 
       [
         "test1.oasis",
         (fun env flags oasis ->
            assert_flag "devmod" flags;
            assert_alternative
              "At least one of ostest, linuxtest64 and linuxtest32 is defined"
              (List.map
                 (fun nm -> (fun () -> assert_flag nm flags))
                 [
                   "ostest";
                   "linuxtest64";
                   "linuxtest32";
                 ])
              ());

         "test2.oasis",
         (fun env flags oasis ->
            ());

         "test3.oasis",
         (fun env flags oasis ->
            ());
       ]
    )
;;

let () = 
  let tmpfiles =
    [
      in_data "src/toto.ml";
      in_data "src/toto.native";
      in_data "src/stuff/A.cmi";
      in_data "src/stuff/B.cmi";
      in_data "src/stuff/C.cmi";
      in_data "src/stuff/stuff.cma";
      in_data "src/stuff/stuff.cmxa";
    ]
  in

  let () = 
    (* Create temporary file *)
    List.iter (fun fn -> close_out (open_out fn)) tmpfiles;

    at_exit
      (fun () ->
         (* Remove temporary file *)
         List.iter Sys.remove tmpfiles)
  in

    assert_equal
      ~msg:"exit code"
      ~printer:string_of_int 
      0
      (Sys.command 
         "../_build/src/OCamlAutobuild.byte -C ../examples/flags")
;;

