(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Simple environment, allowing only to read values
  *)

module MapString = Map.Make(String)

type t = string MapString.t

(** Environment default file 
  *)
let default_filename =
  Filename.concat 
    (Filename.dirname Sys.argv.(0))
    "setup.data"

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
              let line = 
                input_line chn
              in
                Scanf.sscanf line "%s = %S" 
                  (fun nm vl -> rmp := MapString.add nm vl !rmp)
            done;
            ()
          with End_of_file ->
            close_in chn
        end;
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
