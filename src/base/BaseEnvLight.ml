
(** Simple environment, allowing only to read values
  *)

module MapString = Map.Make(String);;

type t = string MapString.t;;

(** Environment default file 
  *)
let default_filename =
  Filename.concat 
    (Filename.dirname Sys.argv.(0))
    "setup.data"
;;

(** Load environment.
  *)
let load ?(allow_empty=false) ?(filename=default_filename) () =
  if Sys.file_exists filename then
    begin
      let chn =
        open_in_bin filename
      in
      let rmp =
        ref MapString.empty
      in
        begin
          try 
            while true do 
              Scanf.fscanf chn "%s = %S\n" 
                (fun nm vl -> rmp := MapString.add nm vl !rmp)
            done;
            ()
          with End_of_file ->
            ()
        end;
        close_in chn;
        !rmp
    end
  else if allow_empty then
    begin
      MapString.empty
    end
  else
    begin
      failwith 
        (Printf.sprintf 
           "Unable to load environment, the file '%s' doesn't exist."
           filename)
    end
;;

(** Get a variable that evaluate expression that can be found in it (see
    {!Buffer.add_substitute}.
  *)
let var_get name env =
  let rec var_expand str =
    let buff =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute 
        buff
        (fun var -> 
           try 
             var_expand (MapString.find var env)
           with Not_found ->
             failwith 
               (Printf.sprintf 
                  "No variable %s defined when trying to expand %S."
                  var 
                  str))
        str;
      Buffer.contents buff
  in
    var_expand (MapString.find name env)
;;
