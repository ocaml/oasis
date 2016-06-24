
(* OASIS_START *)
(* OASIS_STOP *)

let setup_t =
  {setup_t with
       BaseSetup.configure =
         (fun pkg args ->
            let () = setup_t.BaseSetup.configure pkg args in
            close_out (open_out "foobar"));
       BaseSetup.distclean =
         (fun pkg args -> Sys.remove "foobar")
         :: setup_t.BaseSetup.distclean}


let setup () =  BaseSetup.setup setup_t

let () = setup ();;
