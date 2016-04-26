(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


open OASISUtils


let default_filename =
  lazy (Filename.concat
      (Filename.dirname (Lazy.force BaseEnv.default_filename))
      "setup.log")


module SetTupleString =
  Set.Make
    (struct
      type t = string * string
      let compare (s11, s12) (s21, s22) =
        match String.compare s11 s21 with
          | 0 -> String.compare s12 s22
          | n -> n
    end)


let load () =
  let default_filename = Lazy.force default_filename in
  if Sys.file_exists default_filename then
    begin
      let chn =
        open_in default_filename
      in
      let scbuf =
        Scanf.Scanning.from_file default_filename
      in
      let rec read_aux (st, lst) =
        if not (Scanf.Scanning.end_of_input scbuf) then
          begin
            let acc =
              try
                Scanf.bscanf scbuf "%S %S\n"
                  (fun e d ->
                     let t =
                       e, d
                     in
                     if SetTupleString.mem t st then
                       st, lst
                     else
                       SetTupleString.add t st,
                       t :: lst)
              with Scanf.Scan_failure _ ->
                failwith
                  (Scanf.bscanf scbuf
                     "%l"
                     (fun line ->
                        Printf.sprintf
                          "Malformed log file '%s' at line %d"
                          default_filename
                          line))
            in
            read_aux acc
          end
        else
          begin
            close_in chn;
            List.rev lst
          end
      in
      read_aux (SetTupleString.empty, [])
    end
  else
    begin
      []
    end


let register event data =
  let chn_out =
    open_out_gen [Open_append; Open_creat; Open_text] 0o644
      (Lazy.force default_filename)
  in
  Printf.fprintf chn_out "%S %S\n" event data;
  close_out chn_out


let unregister event data =
  let default_filename = Lazy.force default_filename in
  if Sys.file_exists default_filename then
    begin
      let lst =
        load ()
      in
      let chn_out =
        open_out default_filename
      in
      let write_something =
        ref false
      in
      List.iter
        (fun (e, d) ->
           if e <> event || d <> data then
             begin
               write_something := true;
               Printf.fprintf chn_out "%S %S\n" e d
             end)
        lst;
      close_out chn_out;
      if not !write_something then
        Sys.remove default_filename
    end


let filter events =
  let st_events =
    List.fold_left
      (fun st e ->
         SetString.add e st)
      SetString.empty
      events
  in
  List.filter
    (fun (e, _) -> SetString.mem e st_events)
    (load ())


let exists event data =
  List.exists
    (fun v -> (event, data) = v)
    (load ())
