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

(** Generate package files
    @author Sylvain Le Gall
  *)

open Format
open OASISTypes
open OASISUtils
open OASISFileTemplate
open ODN
open OASISPlugin

let required_modules =
  [
    OASISData.oasissys_ml;
    BaseData.basesysenvironment_ml;
    (* TODO: is this module really required ? *)
    BaseData.basesys_ml;
  ]

(** Generate setup file and the rest of the build system 
  *)
let generate pkg dev setup_fn use_real_oasis_filename = 
  let ctxt, _ = 
    BaseSetup.of_package pkg
  in

  let ctxt =
    (* Change setup_fn name if required *)
    if setup_fn <> BaseSetup.default_fn then
      begin
        let setup_tmpl = 
          BaseSetup.find ctxt
        in
          {ctxt with 
               files = 
                 OASISFileTemplate.replace
                   {setup_tmpl with 
                        tgt_fn = Some setup_fn}
                   ctxt.files}
      end
    else
      ctxt
  in

  let ctxt = 
    (* Use BaseDev if asked to *)
    if dev then 
      fst (BaseDev.make use_real_oasis_filename ctxt pkg)
    else
      ctxt
  in

    if ctxt.error then
      exit 1;

    (* Generate other files *)
    OASISFileTemplate.fold
      (fun tmpl () -> file_generate tmpl)
      ctxt.files
      ();

    (* Do other actions *)
    List.iter
      (fun act -> act ())
      ctxt.other_actions

