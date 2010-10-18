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

(** Create the configure, build and install system 
    @author Sylvain Le Gall
  *)

open MainGettext
open OASISUtils
open SubCommand

let main () =
  let _chngs : OASISFileTemplate.file_generate_change list = 
    BaseGenerate.generate 
      ~backup:false
      ~dev:false
      ~setup_fn:BaseSetup.default_filename
      ~restore:false
      (OASISParse.from_file 
         ~ctxt:!BaseContext.default 
         !ArgCommon.oasis_fn)
  in
    ()

let scmd = 
  {(SubCommand.make 
      "setup" 
      (s_ "Translate _oasis into a build system")
      CLIData.setup_mkd
      main)
     with
         scmd_specs = ArgCommon.oasis_fn_specs}

let () = 
  SubCommand.register scmd
    
