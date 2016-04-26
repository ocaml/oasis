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


TYPE_CONV_PATH "OASISTypes"


type name          = string with odn
type package_name  = string with odn
type url           = string with odn
type unix_dirname  = string with odn
type unix_filename = string with odn
type host_dirname  = string with odn
type host_filename = string with odn
type prog          = string with odn
type arg           = string with odn
type args          = string list with odn
type command_line  = (prog * arg list) with odn


type findlib_name = string with odn
type findlib_full = string with odn


type compiled_object =
  | Byte
  | Native
  | Best
with odn


type dependency =
  | FindlibPackage of findlib_full * OASISVersion.comparator option
  | InternalLibrary of name
with odn


type tool =
  | ExternalTool of name
  | InternalExecutable of name
with odn


type vcs =
  | Darcs
  | Git
  | Svn
  | Cvs
  | Hg
  | Bzr
  | Arch
  | Monotone
  | OtherVCS of url
with odn


type plugin_kind =
  [  `Configure
  | `Build
  | `Doc
  | `Test
  | `Install
  | `Extra
  ]


type plugin_data_purpose =
  [  `Configure
  | `Build
  | `Install
  | `Clean
  | `Distclean
  | `Install
  | `Uninstall
  | `Test
  | `Doc
  | `Extra
  | `Other of string
  ]


type 'a plugin = 'a * name * OASISVersion.t option with odn


type all_plugin = plugin_kind plugin


type plugin_data = (all_plugin * plugin_data_purpose * (unit -> unit)) list


(* END EXPORT *)
(* TODO: really export this *)
let odn_of_plugin_data _ =
  ODN.of_list (fun _ -> ODN.UNT) []
(* START EXPORT *)


type 'a conditional = 'a OASISExpr.choices with odn


type custom =
  {
    pre_command:  (command_line option) conditional;
    post_command: (command_line option) conditional;
  }
with odn


type common_section =
  {
    cs_name: name;
    cs_data: PropList.Data.t;
    cs_plugin_data: plugin_data;
  }
with odn


type build_section =
  {
    bs_build:           bool conditional;
    bs_install:         bool conditional;
    bs_path:            unix_dirname;
    bs_compiled_object: compiled_object;
    bs_build_depends:   dependency list;
    bs_build_tools:     tool list;
    bs_c_sources:       unix_filename list;
    bs_data_files:      (unix_filename * unix_filename option) list;
    bs_ccopt:           args conditional;
    bs_cclib:           args conditional;
    bs_dlllib:          args conditional;
    bs_dllpath:         args conditional;
    bs_byteopt:         args conditional;
    bs_nativeopt:       args conditional;
  }
with odn


type library =
  {
    lib_modules:            string list;
    lib_pack:               bool;
    lib_internal_modules:   string list;
    lib_findlib_parent:     findlib_name option;
    lib_findlib_name:       findlib_name option;
    lib_findlib_containers: findlib_name list;
  } with odn


type object_ =
  {
    obj_modules:            string list;
    obj_findlib_fullname:   findlib_name list option;
  } with odn


type executable =
  {
    exec_custom:          bool;
    exec_main_is:         unix_filename;
  } with odn


type flag =
  {
    flag_description:  string option;
    flag_default:      bool conditional;
  } with odn


type source_repository =
  {
    src_repo_type:        vcs;
    src_repo_location:    url;
    src_repo_browser:     url option;
    src_repo_module:      string option;
    src_repo_branch:      string option;
    src_repo_tag:         string option;
    src_repo_subdir:      unix_filename option;
  } with odn


type test =
  {
    test_type:               [`Test] plugin;
    test_command:            command_line conditional;
    test_custom:             custom;
    test_working_directory:  unix_filename option;
    test_run:                bool conditional;
    test_tools:              tool list;
  } with odn


type doc_format =
  | HTML of unix_filename
  | DocText
  | PDF
  | PostScript
  | Info of unix_filename
  | DVI
  | OtherDoc
with odn


type doc =
  {
    doc_type:        [`Doc] plugin;
    doc_custom:      custom;
    doc_build:       bool conditional;
    doc_install:     bool conditional;
    doc_install_dir: unix_filename;
    doc_title:       string;
    doc_authors:     string list;
    doc_abstract:    string option;
    doc_format:      doc_format;
    doc_data_files:  (unix_filename * unix_filename option) list;
    doc_build_tools: tool list;
  } with odn


type section =
  | Library    of common_section * build_section * library
  | Object     of common_section * build_section * object_
  | Executable of common_section * build_section * executable
  | Flag       of common_section * flag
  | SrcRepo    of common_section * source_repository
  | Test       of common_section * test
  | Doc        of common_section * doc
with odn


type section_kind =
  [ `Library | `Object | `Executable | `Flag | `SrcRepo | `Test | `Doc ]


type package =
  {
    oasis_version:          OASISVersion.t;
    ocaml_version:          OASISVersion.comparator option;
    findlib_version:        OASISVersion.comparator option;
    alpha_features:         string list;
    beta_features:          string list;
    name:                   package_name;
    version:                OASISVersion.t;
    license:                OASISLicense.t;
    license_file:           unix_filename option;
    copyrights:             string list;
    maintainers:            string list;
    authors:                string list;
    homepage:               url option;
    bugreports:             url option;
    synopsis:               string;
    description:            OASISText.t option;
    tags:                   string list;
    categories:             url list;

    conf_type:              [`Configure] plugin;
    conf_custom:            custom;

    build_type:             [`Build] plugin;
    build_custom:           custom;

    install_type:           [`Install] plugin;
    install_custom:         custom;
    uninstall_custom:       custom;

    clean_custom:           custom;
    distclean_custom:       custom;

    files_ab:               unix_filename list;
    sections:               section list;
    plugins:                [`Extra] plugin list;
    disable_oasis_section:  unix_filename list;
    schema_data:            PropList.Data.t;
    plugin_data:            plugin_data;
  } with odn


(* END EXPORT *)


type 'a quickstart_level =
  | NoChoice of 'a (* Don't ask question, use provided value *)
  | Beginner
  | Intermediate
  | Expert


type 'a quickstart_question =
  | Field
  | Text
  | Choices of 'a list
  | ExclusiveChoices of 'a list
