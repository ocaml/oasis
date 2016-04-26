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


module MapString = Map.Make(String)


type t = string MapString.t


let default_filename =
  lazy (Filename.concat (Sys.getcwd ()) "setup.data")


let load ?(allow_empty=false) ?(filename=Lazy.force default_filename) () =
  if Sys.file_exists filename then
    begin
      let chn =
        open_in_bin filename
      in
      let st =
        Stream.of_channel chn
      in
      let line =
        ref 1
      in
      let st_line =
        Stream.from
          (fun _ ->
             try
               match Stream.next st with
                 | '\n' -> incr line; Some '\n'
                 | c -> Some c
             with Stream.Failure -> None)
      in
      let lexer =
        Genlex.make_lexer ["="] st_line
      in
      let rec read_file mp =
        match Stream.npeek 3 lexer with
          | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
            Stream.junk lexer;
            Stream.junk lexer;
            Stream.junk lexer;
            read_file (MapString.add nm value mp)
          | [] ->
            mp
          | _ ->
            failwith
              (Printf.sprintf
                 "Malformed data file '%s' line %d"
                 filename !line)
      in
      let mp =
        read_file MapString.empty
      in
      close_in chn;
      mp
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


let rec var_expand str env =
  let buff =
    Buffer.create ((String.length str) * 2)
  in
  Buffer.add_substitute
    buff
    (fun var ->
       try
         var_expand (MapString.find var env) env
       with Not_found ->
         failwith
           (Printf.sprintf
              "No variable %s defined when trying to expand %S."
              var
              str))
    str;
  Buffer.contents buff


let var_get name env =
  var_expand (MapString.find name env) env


let var_choose lst env =
  OASISExpr.choose
    (fun nm -> var_get nm env)
    lst
