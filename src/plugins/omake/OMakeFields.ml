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

(** Describe fields of OMake plugins
    @author Gerd Stolpmann
*)

open BaseEnv
open OASISGettext
open OASISTypes


    TYPE_CONV_PATH "OMakeFields"

type run_t =
  {
    run_path : unix_dirname;   (* "doc" only *)
    extra_args: string list;
  } with odn

let string_of_format =
  function
    | HTML _ -> "html"
    | DocText -> "txt"
    | PDF -> "pdf"
    | PostScript -> "ps"
    | Info _ -> "texi"
    | DVI -> "dvi"
    | OtherDoc -> "html"


(* END EXPORT *)

open OASISTypes
open OASISValues
open OASISPlugin
open OASISGettext
open OASISSchema

let nm, ver =
  "OMake", Some OASISConf.version_short

module CommonFields = struct
  let common_fields schm id pivot_data =
    let extra_args =
      new_field
        schm
        id
        "ExtraArgs"
        ~default:[]
        command_line_options
        (fun () -> s_ "Gives extra arguments to omake")
        pivot_data
        (fun _ t -> t.extra_args) in
    extra_args
end

module BuildFields = struct
  let build_plugin = `Build, nm, ver
  let build_data   = (data_new_property build_plugin : run_t OASISPlugin.prop)
  let self_id, id =
    Build.create build_plugin
  let extra_args =
    CommonFields.common_fields OASISPackage.schema id build_data
end

module DocFields = struct
  type t =
    { path:      unix_dirname;
      modules:   string list;
      texts:     string list;
      libraries: findlib_full list;
      intro:     unix_filename option;
      flags:     string list;
    }

  let doc_plugin = `Doc, nm, ver
  let doc_data = (data_new_property doc_plugin : run_t OASISPlugin.prop)
  let doc_generator_data = data_new_property doc_plugin
  let self_id, id =
    Doc.create doc_plugin

  let new_field nm ?default vl hlp sync =
    new_field
      OASISDocument.schema
      id
      nm
      ?default
      vl
      (fun () -> s_ hlp)
      doc_generator_data sync

  let path =
    new_field
      "Path"
      ~default:OASISUnixPath.current_dir_name
      file
      (ns_ "Top level directory for building ocamldoc documentation")
      (fun _ t -> t.path)

  let modules =
    new_field
      "Modules"
      ~default:[]
      modules
      (ns_ "List of OCaml modules used to generate ocamldoc documentation")
      (fun _ t -> t.modules)

  let texts =
    new_field
      "Texts"
      ~default:[]
      files
      (ns_ "List of text modules used to generate ocamldoc documentation")
      (fun _ t -> t.texts)

  let libraries =
    new_field
      "Libraries"
      ~default:[]
      (comma_separated findlib_full)
      (ns_ "Findlib names of internal libraries used to generate the ocamldoc \
            documentation")
      (fun _ t -> t.libraries)

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

  let extra_args =
    CommonFields.common_fields OASISDocument.schema id doc_data
end

module InstallFields = struct
  let install_plugin = `Install, nm, ver
  let install_data = (data_new_property install_plugin:run_t OASISPlugin.prop)
  let uninstall_data =
    (data_new_property ~purpose:`Uninstall
       install_plugin:run_t OASISPlugin.prop)

  let self_id, id =
    Install.create install_plugin
end
