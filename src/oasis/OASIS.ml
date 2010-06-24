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

(** OASIS library

    @author Sylvain Le Gall
  *)  

open OASISTypes

(** Default configuration for parser/checker *)
let default_conf =
  {
    oasisfn        = None;
    srcdir         = None;
    ignore_unknown = false;
    debug          = false;
  }

(** [from_file ~conf fn] Parse the OASIS file [fn] and check it using
    context [conf].
  *)
let from_file ?(conf=default_conf) fn = 
  let conf =
    (* Add srcdir information to configuration *)
    match conf.srcdir with 
      | Some _ -> conf
      | None -> {conf with srcdir = Some (Filename.dirname fn)}
  in
  let conf =
    (* Add OASIS filename information to configuration *)
    match conf.oasisfn with
      | Some _ -> conf
      | None -> {conf with oasisfn = Some fn}
  in
  let chn =
    open_in fn
  in
  let pkg = 
    OASISAst.to_package 
      conf
      (Stream.of_channel chn)
  in
    close_in chn;
    pkg

(** [from_string ~conf str] Parse the OASIS string [str] and check it using
    context [conf].
  *)
let from_string ?(conf=default_conf) str =
  OASISAst.to_package 
    conf
    (Stream.of_string str)

