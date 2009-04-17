
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

let ocamlc_config_map ocamlc =
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

    var_cache
      "ocamlc_config_map"
      ~hide:true
      (fun env ->
         let ocamlc, env =
           ocamlc env
         in
         let map =
           split_field 
             SMap.empty
             (BaseExec.run_read_output 
                ocamlc
                ["-config"])
         in
         (Marshal.to_string map []), env)
;;

let var_cache ocamlc nm =
  (* Extract data from ocamlc -config *)
  let avlbl_config_get env = 
    let map_marshal, env =
      ocamlc_config_map ocamlc env
    in
      (Marshal.from_string map_marshal 0), env
  in
  let nm_config =
    match nm with 
      | "ocaml_version" -> "version"
      | _ -> nm
  in
    var_cache
      nm 
      (fun env -> 
         try
           let map, env =
             avlbl_config_get env
           in
           let value = 
             SMap.find nm_config map
           in
             value, env
         with Not_found ->
           failwith
             (Printf.sprintf 
                "Cannot find field '%s' in '%s -config' output"
                nm
                (fst (ocamlc env))))
;;

