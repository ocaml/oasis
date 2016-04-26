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


open BaseEnv
open BaseMessage
open OASISTypes
open OASISGettext


let doc lst pkg extra_args =

  let one_doc (doc_plugin, cs, doc) =
    if var_choose
        ~name:(Printf.sprintf
            (f_ "documentation %s build")
            cs.cs_name)
        ~printer:string_of_bool
        doc.doc_build then
      begin
        info (f_ "Building documentation '%s'") cs.cs_name;
        BaseCustom.hook
          doc.doc_custom
          (doc_plugin pkg (cs, doc))
          extra_args
      end
  in
  List.iter one_doc lst;

  if OASISFeatures.package_test OASISFeatures.flag_docs pkg &&
     not (bool_of_string (BaseStandardVar.docs ())) &&
     lst <> [] then
    BaseMessage.warning
      "Docs are turned off, consider enabling with \
       'ocaml setup.ml -configure --enable-docs'"
