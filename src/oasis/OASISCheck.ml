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

(** Check an OASIS package
  *)

open OASISGettext
open OASISSchema
open OASISUtils
open PropList

let check_schema where schm data =
  let msgfld =
    Schema.fold
      (fun acc fld extra hlp ->
         match extra.kind with 
           | DefinePlugin _ ->
               begin
                 acc
               end

           | StandardField ->
               begin
                 try
                   let _ = 
                     Schema.get schm data fld
                   in 
                     acc
                 with 
                   | Not_set _ ->
                       fld :: acc 
                   | No_printer _ ->
                       acc
                   | OASISValues.Not_printable ->
                       acc
               end
           | FieldFromPlugin _ ->
               begin
                 (* TODO: handle plugin checks *)
                 acc
               end)
      []
      schm
  in
    if msgfld <> [] then
      failwithf2
        (f_ "Missing field in %s: %s")
        where
        (String.concat (s_ ", ") msgfld)
