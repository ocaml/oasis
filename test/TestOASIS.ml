
(** Tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open OASISTypes;;
open OASIS;;

let tests ctxt =

  (* Check flag equality *)
  let assert_flag nm oasis =
    try
      let _ = 
        List.find 
          (fun (flg, _) -> nm = flg) 
          oasis.flags
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

  let test_file_of_vector (fn, test) = 
    fn >::
    (fun () ->
       let fn =
         in_data fn
       in
       let oasis =
         from_file 
           ~debug:ctxt.dbug
           ~ignore_unknown:true
           fn
       in
         test oasis)
  in

  let test_value_parser_of_vector (str, value_parse, fail) = 
    str >::
    (fun () ->
       let ctxt =
         {OASISAstTypes.oasisfn = "null";
          srcdir                = "";
          cond                  = None;
          valid_flags           = []}
       in
         try
           ( 
             let _s : string = 
               value_parse ctxt str
             in
               if fail then
                 assert_failure 
                   (Printf.sprintf "Parsing '%s' should have failed" str)
           )
         with _ ->
           (
             if not fail then
               assert_failure
                 (Printf.sprintf "Parsing '%s' should not have failed" str)
           ))
  in

    "OASIS" >:::
    [
      "ValueParser" >:::
      (List.map test_value_parser_of_vector 
         (List.map 
            (fun (v, f) -> (v, OASISValueParser.version_constraint, f))
            [
              ">= 3.11.1", false;
              ">= 3.11",   false;
              "<= 3.11.1", false;
              "> 3.11.1",  false;
              "< 3.11.1",  false;
              "= 3.11.1",  false;
              ">= 3.11 && <= 3.12", false;
              "= 3.11 || = 3.12", false;
              "= || = 3.12", true;
            ])
      );

      "File" >:::
      (List.map test_file_of_vector 
         [
           "test1.oasis",
           (fun oasis ->
              assert_flag "devmod" oasis;
              assert_alternative
                "At least one of ostest, linuxtest64 and linuxtest32 is defined"
                (List.map
                   (fun nm -> (fun () -> assert_flag nm oasis))
                   [
                     "ostest";
                     "linuxtest64";
                     "linuxtest32";
                   ])
                ());

           "test2.oasis",
           ignore;

           "test3.oasis",
           ignore;

           "test4.oasis",
           ignore;
           
           "test5.oasis",
           ignore;
           
           "test6.oasis",
           ignore;

           "test7.oasis",
           ignore;
         ]
      );
    ]
;;
