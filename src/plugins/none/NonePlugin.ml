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

(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISUtils

let not_implemented str _ _ =
  failwithf1 (f_ "No implementation for %s") str

let section_not_implemented str pkg _ _ extra_args =
  not_implemented str pkg extra_args

(* END EXPORT *)

open OASISTypes
open OASISPlugin

module Id = 
struct
  let name = "None"
  let version = OASISConf.version
  let help = NoneData.readme_template_mkd
  let help_extra_vars = []
  let help_order = 10
end

let std_no_generate str pkg =
  {
    moduls       = [NoneData.nonesys_ml];
    setup        = ODNFunc.func_with_arg 
                     not_implemented "NonePlugin.not_implemented"
                     str ODN.of_string;
    clean        = None;
    distclean    = None;
    other_action = ignore;
  },
  pkg

let section_no_generate str pkg (cs, section) =
  let gen, pkg =
    std_no_generate 
      (str^" of section "^cs.cs_name)
      pkg
  in
    gen,
    pkg,
    cs,
    section

let () = 
  let module PU = Configure.Make(Id) in 
    PU.register (std_no_generate "configure");
  let module PU = Build.Make(Id) in 
    PU.register (std_no_generate "build"); 
  let module PU = Doc.Make(Id) in 
    PU.register (section_no_generate "doc"); 
  let module PU = Test.Make(Id) in 
    PU.register (section_no_generate "test"); 
  let module PU = Install.Make(Id) in 
    PU.register (std_no_generate "install", 
                 std_no_generate "uninstall")
