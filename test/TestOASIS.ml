
(** Tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open Oasis;;

let tests ctxt =

  (* Convert a flag to a string *)
  let string_of_flag ?(indent="") flg =
    (match flg.flag_description with
       | Some s -> 
           (Printf.sprintf
              "%sDescription: %s\n"
              indent
              s)
       | None ->
           "")
    ^
    (Printf.sprintf 
       "%sDefault: %b\n" 
       indent
       flg.flag_default)
  in

  (* Convert a list of flags to a string *)
  let string_of_flags ?(indent="") flgs =
    String.concat "\n"
      (List.map
         (fun (nm, flg) ->
            Printf.sprintf "%sFlag %s\n%s"
              indent
              nm
              (string_of_flag ~indent:(indent^" ") flg))
      flgs)
  in

  (* Convert environment to string *)
  let string_of_env env =
    String.concat "; "
      (
        (List.map (fun (nm, b)   -> Printf.sprintf "%s = %b" nm b) env.flag)
        @
        (List.map (fun (nm, str) -> Printf.sprintf "%s = '%s'" nm str) env.test)
      )
  in

  (* Check flag equality *)
  let assert_flag nm flg_exp lst =
    let flg =
      try
        List.assoc nm lst 
      with Not_found ->
        assert_failure 
          (Printf.sprintf 
             "No flag '%s' defined"
             nm)
    in
      assert_equal
        ~printer:string_of_flag
        ~msg:(Printf.sprintf "Checking flag '%s'" nm)
        flg_exp
        flg
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


  "OASIS" >:::
  [
    "test1.oasis" >::
    (fun () ->
       let ast =
         parse_file (in_data "test1.oasis")
       in
       let env =
         env_of_ocamlc "ocamlc"
       in
       let flags =
         flags env ast
       in
       let () = 
         prerr_endline (string_of_env env);
         prerr_endline (string_of_flags flags);
         assert_flag 
           "devmod"
           {
             flag_description = Some "build for developper";
             flag_default     = true;
           }
           flags;
         assert_alternative
           "At least one of ostest, linuxtest64 and linuxtest32 is defined"
           (List.map
              (fun (nm, flg_exp) -> (fun () -> assert_flag nm flg_exp flags))
              [
                "ostest",
                {
                  flag_description = Some "Test on OS";
                  flag_default     = true;
                };
                "linuxtest64",
                {
                  flag_description = Some "Linux 64bits only";
                  flag_default     = true;
                };
                "linuxtest32",
                {
                  flag_description = Some "Linux 32bits only";
                  flag_default     = true;
                };
              ])
           ()
       in
       let _oasis =
         oasis env ast
       in
         ()
    )
  ]
;;

