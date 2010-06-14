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

(* Create documentation using ocamlbuild .odocl files
   @author Sylvain Le Gall
 *)

open OASISTypes
open OASISGettext
open OASISMessage
open BaseStandardVar

TYPE_CONV_PATH "OCamlbuildDocPlugin"

let doc_build path pkg (cs, doc) argv =
  let index_html =
    BaseFilePath.Unix.make
      [
        path;
        cs.cs_name^".docdir";
        "index.html";
      ]
  in
  let tgt_dir =
    BaseFilePath.make
      [
        OCamlbuildCommon.build_dir argv;
        BaseFilePath.of_unix path;
        cs.cs_name^".docdir";
      ]
  in
    OCamlbuildCommon.run_ocamlbuild [index_html] argv;
    List.iter
      (fun glb ->
         BaseBuilt.register
           BaseBuilt.BDoc
           cs.cs_name
           (BaseFileUtil.glob 
              (Filename.concat tgt_dir glb)))
      ["*.html"; "*.css"]

let doc_clean t pkg (cs, doc) argv =
  OCamlbuildCommon.run_clean argv;
  BaseBuilt.unregister BaseBuilt.BDoc cs.cs_name

(* END EXPORT *)

open OASISFileTemplate
open OASISPlugin
open OASISValues
open OASISUtils

module PU = OASISPlugin.Doc.Make(OCamlbuildId)
open PU

let path =
  new_field
    OASISDocument.schema
    "Path"
    directory
    (fun () ->
       s_ "Top level directory for building ocamldoc documentation")


let modules =
  new_field
    OASISDocument.schema
    "Modules"
    ~default:[]
    modules
    (fun () ->
       s_ "List of OCaml modules used to generate ocamldoc documentation")

let libraries =
  new_field
    OASISDocument.schema
    "Libraries"
    ~default:[]
    (comma_separated pkgname)
    (fun () ->
       s_ "Findlib names of internal libraries used to generate the ocamldoc documentation")

(* TODO: the following 2 options require to edit _tags after OCamlbuildDoc
 *)
let intro =
  new_field 
    OASISDocument.schema
    "Intro"
    (opt file)
    (fun () ->
       s_ "OCamldoc formatted file used to generate index.html of the ocamldoc documentation")

let flags =
  new_field
    OASISDocument.schema
    "Flags"
    space_separated 
    (fun () ->
       s_ "OCamldoc flags")

let doit pkg (cs, doc) = 

  let path =
    path cs.cs_data
  in

  let modules_from_libraries =
    (* Convert findlib name to internal library and compute 
     * the module they shipped 
     *)
    let lib_of_findlib =
      let name_of_findlib =
        OASISLibrary.name_findlib_map pkg
      in
      let lib_of_name =
        List.fold_left
          (fun mp ->
             function
               | Library ({cs_name = name}, bs, lib) ->
                   MapString.add name (bs, lib) mp
               | _ ->
                   mp)
          MapString.empty
          pkg.sections
      in
        fun fndlb_nm ->
          try
            let nm = 
              MapString.find fndlb_nm name_of_findlib
            in
              MapString.find nm lib_of_name
          with Not_found ->
            failwithf1
              (f_ "Findlib library %s is not an internal library")
              fndlb_nm
    in

    let fake_root =
      FilePath.UnixPath.make_absolute "/fake_root/"
    in

    let fake_doc_path =
      fake_root path
    in

      (* Fetch modules from internal libraries *)
      List.flatten
        (List.map
           (fun fndlb_nm ->
              let bs, lib =
                lib_of_findlib fndlb_nm
              in
                (* Rebase modules in the doc path *)
                List.map
                  (fun modul ->
                     FilePath.UnixPath.make_relative
                       fake_doc_path
                       (fake_root
                          (FilePath.UnixPath.concat 
                             bs.bs_path
                             modul)))
                  lib.lib_modules)

           (libraries cs.cs_data))
  in

  let modules_from_doc = 
    (* Fetch modules defined directly *)
    modules cs.cs_data
  in

  let modules = 
    modules_from_libraries @ modules_from_doc
  in

  let create_ocamlbuild_odocl_files () =
     file_generate
       (file_make 
          (FilePath.add_extension 
             (FilePath.concat path cs.cs_name)
             "odocl")
          comment_ocamlbuild
          []
          modules
          [])
  in

    (* Checks consistency of options *)
    if List.mem (ExternalTool "ocamldoc") doc.doc_build_tools then
      (* TODO: create a specific error context for this *)
      error ~exit:false 
        (f_ "ocamldoc in field BuildTools of document %s is mandatory.")
        cs.cs_name;
    if List.mem (ExternalTool "ocamlbuild") doc.doc_build_tools then
      (* TODO: create a specific error context for this *)
      error ~exit:false
        (f_ "ocamlbuild in field BuildTools of document %s is mandatory.")
        cs.cs_name;
    if modules = [] then
      error ~exit:false
        (f_ "No module defined for document %s.")
        cs.cs_name;

    {
      moduls = 
        [OCamlbuildData.ocamlbuildsys_ml];

      setup = 
        ODNFunc.func_with_arg 
          doc_build "OCamlbuildDocPlugin.doc_build"
          path ODN.of_string;

      clean = 
        Some 
          (ODNFunc.func_with_arg
             doc_clean "OCamlbuildDocPlugin.doc_clean"
             path ODN.of_string);

      distclean = 
        None;

      other_action = 
        create_ocamlbuild_odocl_files;
    }

let init () = 
  register doit
