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

type t =
    {
      path:    dirname;
      modules: string list;
    } with odn

let doc_build t pkg (cs, doc) argv =
  let index_html =
    t.path^"/"^cs.cs_name^".docdir/index.html"
  in
  let tgt_dir =
    BaseFilePath.make
      [
        OCamlbuildCommon.build_dir argv;
        BaseFilePath.of_unix t.path;
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

open BaseFileGenerate
open OASISPlugin
open OASISValues

(* Field to store precomputed data *)
let auto_doc_data: (unit, t, OASISSchema.extra) PropList.Field.t =
  OASISSchema.new_field_phantom ()

module PU = OASISPlugin.Doc.Make(OCamlbuildId)
open PU

let auto_doc =
  new_field
    OASISLibrary.schema
    "AutoDoc"
    ~default:true
    boolean
    (fun () ->
       s_ "Build documentation associated to the library.")

let of_data = 
  let path =
    new_field
      OASISDocumentation.schema
      "Path"
      directory
      (fun () ->
         s_ "Top level directory for building ocamldoc documentation")
  in

  let modules =
    new_field
      OASISDocumentation.schema
      "Modules"
      modules
      (fun () ->
         s_ "List of OCaml modules used to generate ocamldoc documentation")
  in

    (fun data ->
       let default_not_set f d =
         try 
           f data
         with PropList.Not_set _ ->
           d data
       in
       let t = 
         default_not_set 
           (fun data ->
              PropList.Field.fget data auto_doc_data)
           (fun cs_data ->
              {
                path    = path data;
                modules = modules data;
              })
       in
         {
           path    = default_not_set path (fun _ -> t.path);
           modules = default_not_set modules (fun _ -> t.modules)
         })

let create_ocamlbuild_odocl_files cs t () =
   if t.modules = [] then
     warning 
       (f_ "No module defined for documentation of library %s")
       cs.cs_name;
   file_generate
     (FilePath.add_extension 
        (FilePath.concat t.path cs.cs_name)
        "odocl")
     comment_ocamlbuild
     (Split ([], t.modules, []))

(** Automatically add a doc section to generate API reference out of libraries
  *)
let auto_doc_section pkg = 
  let docs = 
    List.fold_left
      (fun docs ->
         function
           | Library (cs, bs, lib) when auto_doc cs.cs_data ->
               begin
                 let data =
                   PropList.Data.create ()
                 in
                   PropList.Field.fset
                     data
                     auto_doc_data
                     {
                       path    = bs.bs_path;
                       modules = lib.lib_modules;
                     };
                   Doc ({cs_name = cs.cs_name;
                         cs_data = data},
                        {
                          doc_type        = OASISPlugin.builtin "ocamlbuild";
                          doc_build       = bs.bs_build;
                          doc_install     = bs.bs_install;
                          doc_install_dir = "$htmldir/"^cs.cs_name;
                          doc_data_files  = [];
                          doc_build_tools = [];
                        }) :: docs
               end
           | _ ->
               docs)
      []
      pkg.sections
  in
    {pkg with 
         sections = pkg.sections @ (List.rev docs)}

let () = 
  let doit pkg (cs, doc) = 
    let t = 
      of_data cs.cs_data
    in
      {
        moduls = 
          [OCamlbuildData.ocamlbuildsys_ml];

        setup = 
          ODNFunc.func_with_arg 
            doc_build "OCamlbuildDocPlugin.doc_build"
            t odn_of_t;

        clean = 
          Some 
            (ODNFunc.func_with_arg
               doc_clean "OCamlbuildDocPlugin.doc_clean"
               t odn_of_t);

        distclean = 
          None;

        other_action = 
          create_ocamlbuild_odocl_files cs t;
      },
      pkg,
      cs, 
      {doc with 
           doc_build_tools = (ExternalTool "ocamldoc" 
                              :: 
                              ExternalTool "ocamlbuild"
                              :: 
                              doc.doc_build_tools)}
  in
    register doit
