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

open OASISUtils
open OASISGettext

type t =
    {
      scmd_name:      string;
      scmd_synopsis:  string;
      scmd_help:      string;
      scmd_specs:     (Arg.key * Arg.spec * Arg.doc) list;
      scmd_usage:     string;
      scmd_anon:      string -> unit;
      scmd_main:      unit -> unit;
    }

let make ?(std_usage=false) nm snps hlp main =
  {
    scmd_name      = nm;
    scmd_synopsis  = snps;
    scmd_help      = hlp;
    scmd_specs     = [];
    scmd_usage     = if std_usage then s_ "[options*]" else "";
    scmd_anon      = (failwithf (f_ "Don't know what to do with '%s'"));
    scmd_main      = main;
  }

module Set = Set.Make (
struct
  type t' = t
  type t = t'
  let compare t1 t2 = String.compare t1.scmd_name t2.scmd_name
end)

let all =
  ref Set.empty

let register t = 
  all := Set.add t !all

let fold f acc =
  Set.fold f !all acc

let find nm =
  let res = 
    fold 
      (fun c acc ->
         if acc = None && c.scmd_name = nm then
           Some c
         else
           acc)
      None
  in
    match res with 
      | Some c -> 
          c
      | None -> 
          failwithf
            (f_ "Subcommand '%s' doesn't exist")
            nm
