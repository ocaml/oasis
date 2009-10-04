
(** Read output of command ocamlc -config and transform it
  * into enviornment variable
  *)

open BaseEnvironment;;

module SMap = Map.Make(String);;

let ocamlc = BaseCheck.prog_opt "ocamlc";;

let ocamlc_config_map env =
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
                        String.sub 
                          line 
                          (pos_semicolon + 2) 
                          (linelen - pos_semicolon - 2)
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

    var_define
      "ocamlc_config_map"
      ~hide:true
      ~dump:false
      (lazy
         (var_protect
            (Marshal.to_string
               (split_field 
                  SMap.empty
                  (BaseExec.run_read_output (ocamlc env) ["-config"]))
               [])))
      env
;;

let var_cache nm env =
  (* Extract data from ocamlc -config *)
  let avlbl_config_get env = 
    let map_marshal =
      ocamlc_config_map env
    in
      Marshal.from_string map_marshal 0
  in
  let nm_config =
    match nm with 
      | "ocaml_version" -> "version"
      | _ -> nm
  in
    var_define
      nm 
      (lazy
        (try
            let map =
              avlbl_config_get env
            in
            let value = 
              SMap.find nm_config map
            in
              value
          with Not_found ->
            failwith
              (Printf.sprintf 
                 "Cannot find field '%s' in '%s -config' output"
                 nm
                 (ocamlc env))))
      env
;;

