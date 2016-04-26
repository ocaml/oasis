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


(** Plugin to handle "none" generation
    @author Sylvain Le Gall
*)


open OASISGettext
open OASISUtils


let not_implemented str _ _ =
  failwithf (f_ "No implementation for %s") str


let section_not_implemented str pkg _ _ extra_args =
  not_implemented str pkg extra_args


(* END EXPORT *)


open OASISTypes
open OASISPlugin


let std_no_generate str ctxt pkg =
  ctxt,
  {
    chng_moduls    = [NoneData.nonesys_ml];
    chng_clean     = None;
    chng_distclean = None;
    chng_main =
      (ODNFunc.func_with_arg
         not_implemented "NonePlugin.not_implemented"
         str ODN.of_string);
  }


let section_no_generate str ctxt pkg (cs, section) =
  std_no_generate
    (str^" of section "^cs.cs_name)
    ctxt
    pkg


let init () =
  let nm, ver = "None", Some OASISConf.version_short in
  let () =
    register_help (`All, nm, ver)
      {(help_default NoneData.readme_template_mkd) with
         help_order = 10}
  in

  let plugin = `Configure, nm, ver in
  let self_id, _ = Configure.create plugin in
  let () = Configure.register_act self_id (std_no_generate "configure") in

  let plugin = `Build, nm, ver in
  let self_id, _ = Build.create plugin in
  let () = Build.register_act self_id (std_no_generate "build") in

  let plugin = `Install, nm, ver in
  let self_id, _ = Install.create plugin in
  let () = Install.register_act self_id
      ((std_no_generate "install"),
       (std_no_generate "uninstall"))
  in

  let plugin = `Test, nm, ver in
  let self_id, _ = Test.create plugin in
  let () = Test.register_act self_id (section_no_generate "test") in

  let plugin = `Doc, nm, ver in
  let self_id, _ = Doc.create plugin in
  let () = Doc.register_act self_id (section_no_generate "doc") in

  ()

