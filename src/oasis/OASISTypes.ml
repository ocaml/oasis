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

module ODN = OASISDataNotation

type name          = string
type package_name  = string
type url           = string
type unix_dirname  = string
type unix_filename = string
type host_dirname  = string
type host_filename = string
type prog          = string
type arg           = string
type args          = string list
type command_line  = (prog * arg list)

let serialize_name          = ODN.string
let serialize_package_name  = ODN.string
let serialize_url           = ODN.string
let serialize_unix_dirname  = ODN.string
let serialize_unix_filename = ODN.string
let serialize_host_dirname  = ODN.string
let serialize_host_filename = ODN.string
let serialize_prog          = ODN.string
let serialize_arg           = ODN.string
let serialize_args          = ODN.(list string)
let serialize_command_line  = ODN.(tuple2 string (list string))

type findlib_name = string
type findlib_full = string

let serialize_findlib_name = ODN.string
let serialize_findlib_full = ODN.string


type compiled_object =
  | Byte
  | Native
  | Best

let serialize_compiled_object = function
  | Byte -> ODN.vrt0 "OASISTypes.Byte"
  | Native -> ODN.vrt0 "OASISTypes.Native"
  | Best -> ODN.vrt0 "OASISTypes.Best"


type dependency =
  | FindlibPackage of findlib_full * OASISVersion.comparator option
  | InternalLibrary of name

let serialize_dependency = function
  | FindlibPackage (f,c) ->
    ODN.vrt2 ODN.string (ODN.option OASISVersion.serialize_comparator) "OASISTypes.FindlibPackage" f c
  | InternalLibrary n ->
    ODN.vrt1 ODN.string "OASISTypes.InternalLibrary" n


type tool =
  | ExternalTool of name
  | InternalExecutable of name

let serialize_tool = function
  | ExternalTool n -> ODN.vrt1 ODN.string "OASISTypes.ExternalTool" n
  | InternalExecutable n -> ODN.vrt1 ODN.string "OASISTypes.InternalExecutable" n

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

let serialize_vcs = function
  | Darcs -> ODN.vrt0 "OASISTypes.Darcs"
  | Git -> ODN.vrt0 "OASISTypes.Git"
  | Svn -> ODN.vrt0 "OASISTypes.Svn"
  | Cvs -> ODN.vrt0 "OASISTypes.Cvs"
  | Hg -> ODN.vrt0 "OASISTypes.Hg"
  | Bzr -> ODN.vrt0 "OASISTypes.Bzr"
  | Arch -> ODN.vrt0 "OASISTypes.Arch"
  | Monotone -> ODN.vrt0 "OASISTypes.Monotone"
  | OtherVCS u -> ODN.vrt1 ODN.string "OASISTypes.OtherVCS" u


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


type 'a plugin = 'a * name * OASISVersion.t option

let serialize_plugin sx =
  ODN.tuple3 sx serialize_name (ODN.option OASISVersion.serialize)

type all_plugin = plugin_kind plugin


type plugin_data = (all_plugin * plugin_data_purpose * (unit -> unit)) list

(* END EXPORT *)
(* TODO: really export this *)
let serialize_plugin_data _ =
  ODN.list (fun _ -> ODN.UNT) []
(* START EXPORT *)


type 'a conditional = 'a OASISExpr.choices

let serialize_conditional = OASISExpr.serialize_choices

type custom =
  {
    pre_command:  (command_line option) conditional;
    post_command: (command_line option) conditional;
  }

let serialize_custom c =
  let field = serialize_conditional (ODN.option serialize_command_line) in
  ODN.REC ("OASISTypes",
    [ "pre_command", field c.pre_command
    ; "post_command", field c.post_command
    ])

type common_section =
  {
    cs_name: name;
    cs_data: PropList.Data.t;
    cs_plugin_data: plugin_data;
  }

let serialize_common_section x =
  ODN.REC ("OASISTypes",
    [ "cs_name", ODN.string x.cs_name
    ; "cs_data", PropList.Data.serialize x.cs_data
    ; "cs_plugin_data", serialize_plugin_data x.cs_plugin_data
    ])

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

let serialize_build_section x =
  let cargs = serialize_conditional serialize_args in
  ODN.(REC ("OASISTypes",
    [ "bs_build", serialize_conditional bool x.bs_build
    ; "bs_install", serialize_conditional bool x.bs_install
    ; "bs_path", serialize_unix_dirname x.bs_path
    ; "bs_compiled_object", serialize_compiled_object x.bs_compiled_object
    ; "bs_build_depends", list serialize_dependency x.bs_build_depends
    ; "bs_build_tools", list serialize_tool x.bs_build_tools
    ; "bs_c_sources", list serialize_unix_filename x.bs_c_sources
    ; "bs_data_files",
      list (tuple2 serialize_unix_filename (option serialize_unix_filename)) x.bs_data_files
    ; "bs_ccopt", cargs x.bs_ccopt
    ; "bs_cclib", cargs x.bs_cclib
    ; "bs_dlllib", cargs x.bs_dlllib
    ; "bs_dllpath", cargs x.bs_dllpath
    ; "bs_byteopt", cargs x.bs_byteopt
    ; "bs_nativeopt", cargs x.bs_nativeopt
    ]))

type library =
  {
    lib_modules:            string list;
    lib_pack:               bool;
    lib_internal_modules:   string list;
    lib_findlib_parent:     findlib_name option;
    lib_findlib_name:       findlib_name option;
    lib_findlib_containers: findlib_name list;
  }

let serialize_library x =
  ODN.(REC ("OASISTypes",
    [ "lib_modules", list string x.lib_modules
    ; "lib_pack", bool x.lib_pack
    ; "lib_internal_modules", list string x.lib_internal_modules
    ; "lib_findlib_parent", option serialize_findlib_name x.lib_findlib_parent
    ; "lib_findlib_name", option serialize_findlib_full x.lib_findlib_name
    ; "lib_findlib_containers", list serialize_findlib_name x.lib_findlib_containers
    ]))

type object_ =
  {
    obj_modules:            string list;
    obj_findlib_fullname:   findlib_name list option;
  }

let serialize_object_ x=
  ODN.(REC ("OASISTypes",
    [ "obj_modules", list string x.obj_modules
    ; "obj_findlib_fullname", option (list serialize_findlib_name) x.obj_findlib_fullname
    ]))

type executable =
  {
    exec_custom:          bool;
    exec_main_is:         unix_filename;
  }

let serialize_executable x =
  ODN.(REC ("OASISTypes",
    [ "exec_custom", bool x.exec_custom
    ; "exec_main_is", serialize_unix_filename x.exec_main_is
    ]))

type flag =
  {
    flag_description:  string option;
    flag_default:      bool conditional;
  }

let serialize_flag x =
  ODN.(REC ("OASISTypes",
    [ "flag_description", option string x.flag_description
    ; "flag_default", serialize_conditional bool x.flag_default
    ] ))

type source_repository =
  {
    src_repo_type:        vcs;
    src_repo_location:    url;
    src_repo_browser:     url option;
    src_repo_module:      string option;
    src_repo_branch:      string option;
    src_repo_tag:         string option;
    src_repo_subdir:      unix_filename option;
  }

let serialize_source_repository x=
  ODN.(REC ("OASISTypes",
    [ "src_repo_type", serialize_vcs x.src_repo_type
    ; "src_repo_location", serialize_url x.src_repo_location
    ; "src_repo_browser", option serialize_url x.src_repo_browser
    ; "src_repo_module", option string x.src_repo_module
    ; "src_repo_branch", option string x.src_repo_branch
    ; "src_repo_tag", option string x.src_repo_tag
    ; "src_repo_subdir", option serialize_unix_filename x.src_repo_subdir
    ]))

type test =
  {
    test_type:               [`Test] plugin;
    test_command:            command_line conditional;
    test_custom:             custom;
    test_working_directory:  unix_filename option;
    test_run:                bool conditional;
    test_tools:              tool list;
  }

let serialize_test x =
  let splugin _ = ODN.PVR ("Test", None) in
  ODN.(REC ("OASISTypes",
    [ "test_type", serialize_plugin splugin x.test_type
    ; "test_command", serialize_conditional serialize_command_line x.test_command
    ; "test_custom", serialize_custom x.test_custom
    ; "test_working_directory", option serialize_unix_filename x.test_working_directory
    ; "test_run", serialize_conditional bool x.test_run
    ; "test_tools", list serialize_tool x.test_tools
    ]))

type doc_format =
  | HTML of unix_filename
  | DocText
  | PDF
  | PostScript
  | Info of unix_filename
  | DVI
  | OtherDoc

let serialize_doc_format = function
  | HTML f -> ODN.vrt1 serialize_unix_filename "OASISTypes.HTML" f
  | DocText -> ODN.vrt0 "OASISTypes.DocText"
  | PDF -> ODN.vrt0 "OASISTypes.PDF"
  | PostScript -> ODN.vrt0 "OASISTypes.PostScript"
  | Info f -> ODN.vrt1 serialize_unix_filename "OASISTypes.Info" f
  | DVI -> ODN.vrt0 "OASISTypes.DVI"
  | OtherDoc -> ODN.vrt0 "OASISTypes.OtherDoc"

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
  }

let serialize_doc x =
  let splugin _ = ODN.PVR ("Doc", None) in
  ODN.(REC ("OASISTypes",
    [ "doc_type", serialize_plugin splugin x.doc_type
    ; "doc_custom", serialize_custom x.doc_custom
    ; "doc_build", serialize_conditional bool x.doc_build
    ; "doc_install", serialize_conditional bool x.doc_install
    ; "doc_install_dir", serialize_unix_filename x.doc_install_dir
    ; "doc_title", string x.doc_title
    ; "doc_authors", list string x.doc_authors
    ; "doc_abstract", option string x.doc_abstract
    ; "doc_format", serialize_doc_format x.doc_format
    ; "doc_data_files",
        list (tuple2 serialize_unix_filename (option serialize_unix_filename)) x.doc_data_files
    ; "doc_build_tools", list serialize_tool x.doc_build_tools
    ] ))

type section =
  | Library    of common_section * build_section * library
  | Object     of common_section * build_section * object_
  | Executable of common_section * build_section * executable
  | Flag       of common_section * flag
  | SrcRepo    of common_section * source_repository
  | Test       of common_section * test
  | Doc        of common_section * doc

let serialize_section =
  let cs = serialize_common_section in
  let bs = serialize_build_section in
  function
    | Library (c,b,l) -> ODN.vrt3 cs bs serialize_library "OASISTypes.Library" c b l
    | Object (c,b,o) -> ODN.vrt3 cs bs serialize_object_ "OASISTypes.Object" c b o
    | Executable (c,b,e) -> ODN.vrt3 cs bs serialize_executable "OASISTypes.Executable" c b e
    | Flag (c,f) -> ODN.vrt2 cs serialize_flag "OASISTypes.Flag" c f
    | SrcRepo (c,s) -> ODN.vrt2 cs serialize_source_repository "OASISTypes.SrcRepo" c s
    | Test (c,t) -> ODN.vrt2 cs serialize_test "OASISTypes.Test" c t
    | Doc (c,d) -> ODN.vrt2 cs serialize_doc "OASISTypes.Doc" c d

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
  }

let serialize_package (x:package) =
  let spvr n _ = ODN.PVR (n, None) in
  ODN.(REC ("OASISTypes",
    [ "oasis_version", OASISVersion.serialize x.oasis_version
    ; "ocaml_version", option OASISVersion.serialize_comparator x.ocaml_version
    ; "findlib_version", option OASISVersion.serialize_comparator x.findlib_version
    ; "alpha_features", list string x.alpha_features
    ; "beta_features", list string x.beta_features
    ; "name", serialize_package_name x.name
    ; "version", OASISVersion.serialize x.version
    ; "license", OASISLicense.serialize x.license
    ; "license_file", option serialize_unix_filename x.license_file
    ; "copyrights", list string x.copyrights
    ; "maintainers", list string x.maintainers
    ; "authors", list string x.authors
    ; "homepage", option serialize_url x.homepage
    ; "bugreports", option serialize_url x.bugreports
    ; "synopsis", string x.synopsis
    ; "description", option OASISText.serialize x.description
    ; "tags", list string x.tags
    ; "categories", list serialize_url x.categories
    ; "conf_type", serialize_plugin (spvr "Configure") x.conf_type
    ; "conf_custom", serialize_custom x.conf_custom
    ; "build_type", serialize_plugin (spvr "Build") x.build_type
    ; "build_custom", serialize_custom x.build_custom
    ; "install_type", serialize_plugin (spvr "Install") x.install_type
    ; "install_custom", serialize_custom x.install_custom
    ; "uninstall_custom", serialize_custom x.uninstall_custom
    ; "clean_custom", serialize_custom x.clean_custom
    ; "distclean_custom", serialize_custom x.distclean_custom
    ; "files_ab", list serialize_unix_filename  x.files_ab
    ; "sections", list serialize_section x.sections
    ; "plugins", list (serialize_plugin (spvr "Extra")) x.plugins
    ; "disable_oasis_section", list serialize_unix_filename x.disable_oasis_section
    ; "schema_data", PropList.Data.serialize x.schema_data
    ; "plugin_data", serialize_plugin_data x.plugin_data
    ]))


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
