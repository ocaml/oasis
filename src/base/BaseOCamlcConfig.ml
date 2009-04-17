
(** Read output of command ocamlc -config and transform it
  * into enviornment variable
  *)

open BaseEnvironment;;

module SMap = Map.Make(String);;

let config =
  [
    "os_type";
    "system";
    "architecture";
    "ccomp_type";
    "ocaml_version";
  ]
;;

let init ocamlc_get env =
  (* Map name to value for ocamlc -config output 
     (name ^": "^value) 
   *)
  let rec split_field mp lst = 
    match lst with 
      | line :: tl ->
          let mp =
            try
              let pos_semicolon =
                String.index line ':'
              in
                if pos_semicolon > 1 then            
                  (
                    let name =
                      String.sub line 0 pos_semicolon 
                    in
                    let linelen =
                      String.length line
                    in
                    let value =
                      if linelen > pos_semicolon + 2 then
                        String.sub line (pos_semicolon + 2) (linelen - pos_semicolon - 2)
                      else
                        ""
                    in
                      SMap.add name value mp
                  )
                else
                  (
                    mp
                  )
            with Not_found ->
              (
                mp
              )
          in
            split_field mp tl
      | [] ->
          mp
  in

  let avlbl_config =
    ref None
  in

  (* Extract data from ocamlc -config *)
  let avlbl_config_get env = 
    if !avlbl_config = None then
      (
        avlbl_config := 
          Some
          (split_field 
             SMap.empty
             (BaseExec.run_read_output 
                (ocamlc_get env)
                ["-config"]))
      );
    match !avlbl_config with
      | Some e -> e
      | None ->
          failwith 
            (Printf.sprintf
               "Unable to compute '%s -config'"
               (ocamlc_get env))
  in

    (* Set available test/var *)
    List.fold_left
      (fun env nm ->
           let nm_config =
             match nm with 
               | "ocaml_version" -> "version"
               | _ -> nm
           in
             var_define 
               nm 
               (fun env -> 
                  try
                    Msg.checking nm;
                    Msg.result_wrap 
                      (SMap.find nm_config 
                         (avlbl_config_get env))
                  with Not_found ->
                    failwith
                      (Printf.sprintf 
                         "Cannot find field '%s' in '%s -config' output"
                         nm
                         (ocamlc_get env)))
               env)
      env
      config
;;

