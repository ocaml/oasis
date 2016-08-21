(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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
open OASISContext
open OASISGettext
open OASISFileSystem


let default_filename = in_srcdir "setup.log"


let load ~ctxt () =
  let module SetTupleString =
    Set.Make
      (struct
        type t = string * string
        let compare (s11, s12) (s21, s22) =
          match String.compare s11 s21 with
          | 0 -> String.compare s12 s22
          | n -> n
      end)
  in
  if ctxt.srcfs#file_exists default_filename then begin
    defer_close
      (ctxt.srcfs#open_in default_filename)
      (fun rdr ->
         let line = ref 1 in
         let lxr = Genlex.make_lexer [] (stream_of_reader rdr) in
         let rec read_aux (st, lst) =
           match Stream.npeek 2 lxr with
           | [Genlex.String e; Genlex.String d] ->
             let t = e, d in
             Stream.junk lxr; Stream.junk lxr;
             if SetTupleString.mem t st then
               read_aux (st, lst)
             else
               read_aux (SetTupleString.add t st, t :: lst)
           | [] -> List.rev lst
           | _ ->
             failwithf
               (f_ "Malformed log file '%s' at line %d")
               (ctxt.srcfs#string_of_filename default_filename)
               !line
         in
         read_aux (SetTupleString.empty, []))
  end else begin
    []
  end


let register ~ctxt event data =
  defer_close
    (ctxt.srcfs#open_out
       ~mode:[Open_append; Open_creat; Open_text]
       ~perm:0o644
       default_filename)
    (fun wrtr ->
       let buf = Buffer.create 13 in
       Printf.bprintf buf "%S %S\n" event data;
       wrtr#output buf)


let unregister ~ctxt event data =
  let lst = load ~ctxt () in
  let buf = Buffer.create 13 in
  List.iter
    (fun (e, d) ->
       if e <> event || d <> data then
         Printf.bprintf buf "%S %S\n" e d)
    lst;
  if Buffer.length buf > 0 then
    defer_close
      (ctxt.srcfs#open_out default_filename)
      (fun wrtr -> wrtr#output buf)
  else
    ctxt.srcfs#remove default_filename


let filter ~ctxt events =
  let st_events = SetString.of_list events in
  List.filter
    (fun (e, _) -> SetString.mem e st_events)
    (load ~ctxt ())


let exists ~ctxt event data =
  List.exists
    (fun v -> (event, data) = v)
    (load ~ctxt ())
