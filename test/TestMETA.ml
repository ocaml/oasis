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


(** Test plugin META
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open Fl_metascanner
open OASISTypes
open OASISFindlib


let tests =
  let test_of_vector (nm, oasis_str, pkg_tests) =
    nm >::
    (fun test_ctxt ->
       let fn, _ =
         bracket_tmpfile ~prefix:"oasis-meta-" ~suffix:".meta" test_ctxt
       in
       (* Parse string to get OASIS package *)
       let pkg =
         OASISParse.from_string
           ~ctxt:oasis_ctxt
           oasis_str
       in

       (* Generate META file *)
       let groups, findlib_name_of_library_name, _ =
         OASISFindlib.findlib_mapping pkg
       in
       let write_meta fndlb_nm =
         let grp =
           try
             (* Find the package in all group *)
             List.find
               (fun grp ->
                  match grp with
                    | Container (nm, _)
                    | Package (nm, _, _, _, _) -> nm = fndlb_nm)
               groups
           with Not_found ->
             failwith
               (Printf.sprintf
                  "Cannot find group of name '%s'"
                  fndlb_nm)
         in
         let chn =
           open_out fn
         in
         let fmt =
           Format.formatter_of_out_channel chn
         in
         let root_t =
           let root_cs, _, _ =
             root_of_group grp
           in
             METAPlugin.generator root_cs.cs_data
         in
           METAPlugin.pp_print_meta
             pkg root_t findlib_name_of_library_name fmt grp;
           close_out chn
       in

       (* Check META file *)
       let rec find_pkg_defs pkg_expr =
         function
           | hd :: tl ->
               begin
                 try
                   find_pkg_defs
                     (List.assoc hd pkg_expr.pkg_children)
                     tl
                 with Not_found ->
                   failwith
                     (Printf.sprintf
                        "Could not find subpackage component '%s'"
                        hd)
               end
           | [] ->
               pkg_expr.pkg_defs
       in
       let Some (_, _) | None =
         List.fold_left
           (fun former_meta (pkg_name, var, preds, res) ->
              let pkg_root, pkg_paths =
                match OASISString.nsplit pkg_name '.' with
                  | hd :: tl -> hd, tl
                  | _ -> assert(false)
              in
              let pkg_expr =
                match former_meta with
                  | Some (nm, pkg_expr) when nm = pkg_root ->
                      pkg_expr
                  | _ ->
                      begin
                        let chn =
                          write_meta pkg_root;
                          dbug_file_content test_ctxt fn;
                          open_in fn
                        in
                        let res =
                          parse chn
                        in
                          close_in chn;
                          res
                      end
              in
              let pkg_defs =
                find_pkg_defs pkg_expr pkg_paths
              in
                begin
                  let msg =
                    Printf.sprintf
                      "%s %s(%s)"
                      pkg_name
                      var
                      (String.concat "," preds)
                  in
                    try
                      assert_equal
                        ~msg
                        ~printer:(fun s -> s)
                        res
                        (lookup var preds pkg_defs)
                    with Not_found ->
                      failwith
                        (Printf.sprintf
                           "Cannot find META variable '%s'"
                           msg)
                end;
                Some (pkg_root, pkg_expr))
           None
           pkg_tests
       in
         ())
  in

    "META" >:::
    (List.map test_of_vector
       [
(* TODO: move that to files. *)
         "2-subpackages",
         "\
OASISFormat:  0.1
Name:         ocaml-data-notation
Version:      0.0.1
Synopsis:     store data using OCaml notation
License:      LGPL with OCaml linking exception
Authors:      me

Library odn
  Path:    src
  Modules: ODN

Library pa_odn
  Path:              src
  Modules:           Pa_odn
  FindlibParent:     odn
  XMETADescription:  Syntax extension for odn
  FindlibContainers: with
  FindlibName:       syntax

Library pa_noodn
  Path:              src
  Modules:           Pa_noodn
  FindlibParent:     odn
  XMETADescription:  Syntax extension that removes 'with odn'
  FindlibContainers: without
  FindlibName:       syntax",
         [
           "odn", "archive", ["byte"], "odn.cma";
           "odn.with.syntax", "archive", ["byte"], "pa_odn.cma";

           "odn.without.syntax", "description", [],
           "Syntax extension that removes 'with odn'";
         ];

         "virtual-root",
         "\
OASISFormat:  0.1
Name:         ocaml-data-notation
Version:      0.0.1
Synopsis:     store data using OCaml notation
License:      LGPL with OCaml linking exception
Authors:      me

Library odn
  Path:    src
  Modules: ODN
  FindlibContainers:  myext.toto",
         [
           "myext.toto.odn", "archive", ["byte"], "odn.cma";
         ];

         "syntax",
         "\
OASISFormat:  0.1
Name:         ocaml-data-notation
Version:      0.0.1
Synopsis:     store data using OCaml notation
License:      LGPL with OCaml linking exception
Authors:      me

Library odn
  Path:    src
  Modules: ODN

Library pa_odn
  Path:             src
  Modules:          Pa_odn
  FindlibParent:    odn
  XMETADescription: Syntax extension for odn
  XMETAType:        syntax
  XMETARequires:    type-conv.syntax, camlp4
  FindlibName:      syntax",
         [
           "odn.syntax", "archive", ["syntax"; "preprocessor"], "pa_odn.cma";
           "odn.syntax", "archive", ["syntax"; "toploop"], "pa_odn.cma";
           "odn.syntax", "requires", [], "type-conv.syntax camlp4";
         ];

         "long-synopsis",
         "\
OASISFormat:  0.1
Name:         ocaml-data-notation
Version:      0.0.1
Synopsis:     store data using OCaml notation with a very very very very very \
                                                     very very long synopsis
              and with line breaks
License:      LGPL with OCaml linking exception
Authors:      me

Library odn
  Path:    src
  Modules: ODN",
         [
           "odn", "archive", ["byte"], "odn.cma";
         ];

       ])
