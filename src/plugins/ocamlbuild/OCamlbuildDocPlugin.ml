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


(* Create documentation using ocamlbuild .odocl files
   @author Sylvain Le Gall
*)


open OASISTypes
open OASISGettext
open OASISMessage
open OCamlbuildCommon
open BaseStandardVar


    TYPE_CONV_PATH "OCamlbuildDocPlugin"

type run_t =
  {
    extra_args: string list;
    run_path: unix_filename;
  } with odn


let doc_build run pkg (cs, doc) argv =
  let index_html =
    OASISUnixPath.make
      [
        run.run_path;
        cs.cs_name^".docdir";
        "index.html";
      ]
  in
  let tgt_dir =
    OASISHostPath.make
      [
        build_dir argv;
        OASISHostPath.of_unix run.run_path;
        cs.cs_name^".docdir";
      ]
  in
  run_ocamlbuild (index_html :: run.extra_args) argv;
  List.iter
    (fun glb ->
       BaseBuilt.register
         BaseBuilt.BDoc
         cs.cs_name
         [OASISFileUtil.glob ~ctxt:!BaseContext.default
            (Filename.concat tgt_dir glb)])
    ["*.html"; "*.css"]


let doc_clean run pkg (cs, doc) argv =
  run_clean argv;
  BaseBuilt.unregister BaseBuilt.BDoc cs.cs_name


(* END EXPORT *)


open OASISFileTemplate
open OASISPlugin
open OASISValues
open OASISUtils
open OASISPlugin
open OASISSchema
open OCamlbuildId


let plugin =
  `Doc, name, Some version


type t =
  {
    path:      unix_dirname;
    modules:   string list;
    libraries: findlib_full list;
    intro:     unix_filename option;
    flags:     string list;
    common:    ocamlbuild_common;
  }


let pivot_data = data_new_property plugin
let pivot_sub_data = data_new_property plugin


let self_id, all_id =
  Doc.create plugin


let new_field nm ?default vl hlp sync =
  new_field
    OASISDocument.schema
    all_id
    nm
    ?default
    vl
    (fun () -> s_ hlp)
    pivot_data sync


let path =
  new_field
    "Path"
    directory
    (ns_ "Top level directory for building ocamldoc documentation")
    (fun _ t -> t.path)


let modules =
  new_field
    "Modules"
    ~default:[]
    modules
    (ns_ "List of OCaml modules used to generate ocamldoc documentation")
    (fun _ t -> t.modules)


let libraries =
  new_field
    "Libraries"
    ~default:[]
    (comma_separated findlib_full)
    (ns_ "Findlib names of internal libraries used to generate the ocamldoc \
          documentation")
    (fun _ t -> t.libraries)


(* TODO: the following 2 options require to edit _tags after OCamlbuildDoc
*)
(*
let intro =
  new_field
    ~default:None
    "Intro"
    (opt file)
    (ns_ "OCamldoc formatted file used to generate index.html of the ocamldoc \
          documentation")
    (fun _ t -> t.intro)


let flags =
  new_field
    ~default:[]
    "Flags"
    space_separated
    (ns_ "OCamldoc flags")
    (fun _ t -> t.flags)
 *)


(* TODO: use -t for title *)


let generator =
  let generator_common =
    (* Register fields. *)
    ocamlbuild_common_generator pivot_sub_data OASISDocument.schema all_id
  in
  fun data pkg ->
    let path = path data in

    let modules_from_libraries =
      (* Convert findlib name to internal library and compute
       * the module they shipped.
      *)
      let lib_of_findlib =
        let _, _, library_name_of_findlib_name =
          OASISFindlib.findlib_mapping pkg
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
          let nm =
            library_name_of_findlib_name fndlb_nm
          in
          MapString.find nm lib_of_name
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
                   OASISUnixPath.make_relative
                     path
                     (OASISUnixPath.concat bs.bs_path modul))
                lib.lib_modules)

           (libraries data))
    in

    let modules_from_doc =
      (* Fetch modules defined directly *)
      modules data
    in

    let modules =
      modules_from_libraries @ modules_from_doc
    in
    {
      path = path;
      modules = modules;
      libraries = libraries data;
      intro = None;
      flags = [];
      common = generator_common data;
    }


let doit ctxt pkg (cs, doc) =
  let t = generator cs.cs_data pkg in

  let ctxt =
    (* Create .odocl file *)
    add_file
      (template_make
         (OASISHostPath.add_extension
            (Filename.concat t.path cs.cs_name)
            "odocl")
         comment_ocamlbuild
         []
         t.modules
         [])
      ctxt
  in

  let ctxt =
    (* Checks consistency of options *)
    (* TODO: merge with qstrt_completion *)
    List.fold_left
      (fun ctxt f -> f ctxt)
      ctxt
      [
        set_error
          (not (List.mem (ExternalTool "ocamldoc") doc.doc_build_tools))
          (Printf.sprintf
             (f_ "ocamldoc in field BuildTools of document %s is mandatory.")
             cs.cs_name);

        set_error
          (not (List.mem (ExternalTool "ocamlbuild") doc.doc_build_tools))
          (Printf.sprintf
             (f_ "ocamlbuild in field BuildTools of document %s is mandatory.")
             cs.cs_name);

        set_error
          (t.modules = [])
          (Printf.sprintf
             (f_ "No module defined for document %s.")
             cs.cs_name);
      ]
  in

  let run =
    {
      run_path = t.path;
      extra_args = extra_args_ocamlbuild_common ~ctxt:ctxt.ctxt pkg t.common;
    }
  in
  ctxt,
  {
    chng_moduls =
      [OCamlbuildData.ocamlbuildsys_ml];

    chng_main =
      ODNFunc.func_with_arg
        doc_build "OCamlbuildDocPlugin.doc_build"
        run odn_of_run_t;

    chng_clean =
      Some
        (ODNFunc.func_with_arg
           doc_clean "OCamlbuildDocPlugin.doc_clean"
           run odn_of_run_t);

    chng_distclean =
      None;
  }


let qstrt_completion pkg =
  List.fold_left
    (fun pkg tool -> fix_build_tools tool pkg)
    pkg
    [ExternalTool "ocamlbuild";
     ExternalTool "ocamldoc"]


let init () =
  OCamlbuildId.init ();
  Doc.register_act self_id doit;
  register_quickstart_completion all_id qstrt_completion
