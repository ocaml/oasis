(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


type name          = string
type package_name  = string
type url           = string
type unix_dirname  = string
type unix_filename = string (* TODO: replace everywhere. *)
type host_dirname  = string (* TODO: replace everywhere. *)
type host_filename = string (* TODO: replace everywhere. *)
type prog          = string
type arg           = string
type args          = string list
type command_line  = (prog * arg list)


type findlib_name = string
type findlib_full = string


type compiled_object =
  | Byte
  | Native
  | Best


type dependency =
  | FindlibPackage of findlib_full * OASISVersion.comparator option
  | InternalLibrary of name


type tool =
  | ExternalTool of name
  | InternalExecutable of name


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


type all_plugin = plugin_kind plugin


type plugin_data = (all_plugin * plugin_data_purpose * (unit -> unit)) list


type 'a conditional = 'a OASISExpr.choices


type custom =
  {
    pre_command:  (command_line option) conditional;
    post_command: (command_line option) conditional;
  }


type common_section =
  {
    cs_name: name;
    cs_data: PropList.Data.t;
    cs_plugin_data: plugin_data;
  }


type build_section =
  {
    bs_build:                   bool conditional;
    bs_install:                 bool conditional;
    bs_path:                    unix_dirname;
    bs_compiled_object:         compiled_object;
    bs_build_depends:           dependency list;
    bs_build_tools:             tool list;
    bs_interface_patterns:      OASISSourcePatterns.t list;
    bs_implementation_patterns: OASISSourcePatterns.t list;
    bs_c_sources:               unix_filename list;
    bs_data_files:              (unix_filename * unix_filename option) list;
    bs_findlib_extra_files:     unix_filename list;
    bs_ccopt:                   args conditional;
    bs_cclib:                   args conditional;
    bs_dlllib:                  args conditional;
    bs_dllpath:                 args conditional;
    bs_byteopt:                 args conditional;
    bs_nativeopt:               args conditional;
  }


type library =
  {
    lib_modules:            string list;
    lib_pack:               bool;
    lib_internal_modules:   string list;
    lib_findlib_parent:     findlib_name option;
    lib_findlib_name:       findlib_name option;
    lib_findlib_directory:  unix_dirname option;
    lib_findlib_containers: findlib_name list;
  }


type object_ =
  {
    obj_modules:            string list;
    obj_findlib_fullname:   findlib_name list option;
    obj_findlib_directory:  unix_dirname option;
  }


type executable =
  {
    exec_custom:          bool;
    exec_main_is:         unix_filename;
  }


type flag =
  {
    flag_description:  string option;
    flag_default:      bool conditional;
  }


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


type test =
  {
    test_type:               [`Test] plugin;
    test_command:            command_line conditional;
    test_custom:             custom;
    test_working_directory:  unix_filename option;
    test_run:                bool conditional;
    test_tools:              tool list;
  }


type doc_format =
  | HTML of unix_filename (* TODO: source filename. *)
  | DocText
  | PDF
  | PostScript
  | Info of unix_filename (* TODO: source filename. *)
  | DVI
  | OtherDoc


type doc =
  {
    doc_type:        [`Doc] plugin;
    doc_custom:      custom;
    doc_build:       bool conditional;
    doc_install:     bool conditional;
    doc_install_dir: unix_filename; (* TODO: dest filename ?. *)
    doc_title:       string;
    doc_authors:     string list;
    doc_abstract:    string option;
    doc_format:      doc_format;
    (* TODO: src filename. *)
    doc_data_files:  (unix_filename * unix_filename option) list;
    doc_build_tools: tool list;
  }


type section =
  | Library    of common_section * build_section * library
  | Object     of common_section * build_section * object_
  | Executable of common_section * build_section * executable
  | Flag       of common_section * flag
  | SrcRepo    of common_section * source_repository
  | Test       of common_section * test
  | Doc        of common_section * doc


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
    license_file:           unix_filename option; (* TODO: source filename. *)
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

    files_ab:               unix_filename list; (* TODO: source filename. *)
    sections:               section list;
    plugins:                [`Extra] plugin list;
    disable_oasis_section:  unix_filename list; (* TODO: source filename. *)
    schema_data:            PropList.Data.t;
    plugin_data:            plugin_data;
  }


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


let odn_of_unix_dirname = OASISDataNotation.of_string
let odn_of_unix_filename = OASISDataNotation.of_string
let odn_of_conditional = OASISExpr.odn_of_choices

let odn_of_command_line (prog, args) =
  OASISDataNotation.TPL [OASISDataNotation.STR prog; OASISDataNotation.of_list OASISDataNotation.of_string args]

let odn_of_package pkg =
  let open OASISDataNotation in
  let proplist_data = APP ("PropList.Data.create", [], [UNT]) in
  let odn_of_args = of_list of_string in
  let odn_of_plugin odn_of_a (v2, v1, v0) =
    TPL
      [ odn_of_a v2;
        of_string v1;
        of_option OASISVersion.odn_of_t v0]
  in
  let odn_of_custom v =
    REC
    ("OASISTypes",
    ["pre_command", odn_of_conditional (of_option odn_of_command_line) v.pre_command;
    "post_command", odn_of_conditional (of_option odn_of_command_line) v.post_command])
  in
  let odn_of_tool =
    function
    | ExternalTool v0 ->
        VRT ("OASISTypes.ExternalTool", [ STR v0 ])
    | InternalExecutable v0 ->
        VRT ("OASISTypes.InternalExecutable", [ STR v0 ])
  in
  let odn_of_dependency =
    function
    | FindlibPackage ((v1, v0)) ->
        VRT ("OASISTypes.FindlibPackage",
          [ of_string v1; of_option OASISVersion.odn_of_comparator v0 ])
    | InternalLibrary v0 ->
        VRT ("OASISTypes.InternalLibrary", [ STR v0 ])
  in
  let odn_of_compiled_object =
    function
    | Byte -> VRT ("OASISTypes.Byte", [])
    | Native -> VRT ("OASISTypes.Native", [])
    | Best -> VRT ("OASISTypes.Best", [])
  in
  let odn_of_common_section v =
    REC ("OASISTypes",
      [ ("cs_name", (STR v.cs_name));
        ("cs_data", proplist_data);
        ("cs_plugin_data", LST []) ])
  in
  let odn_of_build_section v =
    REC ("OASISTypes",
      [ ("bs_build", (odn_of_conditional of_bool v.bs_build));
        ("bs_install", (odn_of_conditional of_bool v.bs_install));
        ("bs_path", (odn_of_unix_dirname v.bs_path));
        ("bs_compiled_object", (odn_of_compiled_object v.bs_compiled_object));
        ("bs_build_depends",
         (of_list odn_of_dependency v.bs_build_depends));
        ("bs_build_tools",
         (of_list odn_of_tool v.bs_build_tools));
        ("bs_interface_patterns",
         (of_list OASISSourcePatterns.odn_of_t v.bs_interface_patterns));
        ("bs_implementation_patterns",
         (of_list OASISSourcePatterns.odn_of_t v.bs_implementation_patterns));
        ("bs_c_sources",
         (of_list odn_of_unix_filename v.bs_c_sources));
        ("bs_data_files",
         (of_list
            (fun (v1, v0) ->
               TPL
                 [ odn_of_unix_filename v1;
                   of_option odn_of_unix_filename v0 ])
            v.bs_data_files));
        ("bs_findlib_extra_files",
         (of_list odn_of_unix_filename v.bs_findlib_extra_files));
        ("bs_ccopt", (odn_of_conditional odn_of_args v.bs_ccopt));
        ("bs_cclib", (odn_of_conditional odn_of_args v.bs_cclib));
        ("bs_dlllib", (odn_of_conditional odn_of_args v.bs_dlllib));
        ("bs_dllpath", (odn_of_conditional odn_of_args v.bs_dllpath));
        ("bs_byteopt", (odn_of_conditional odn_of_args v.bs_byteopt));
        ("bs_nativeopt", (odn_of_conditional odn_of_args v.bs_nativeopt)) ])
  in
  let odn_of_library v =
    REC ("OASISTypes",
      [ ("lib_modules", of_list of_string v.lib_modules);
        ("lib_pack", of_bool v.lib_pack);
        ("lib_internal_modules",
         of_list of_string v.lib_internal_modules);
        ("lib_findlib_parent",
         of_option of_string v.lib_findlib_parent);
        ("lib_findlib_name",
         of_option of_string v.lib_findlib_name);
        ("lib_findlib_directory",
         of_option odn_of_unix_filename v.lib_findlib_directory);
        ("lib_findlib_containers",
         of_list of_string v.lib_findlib_containers);
      ])
  in
  let odn_of_object_ v =
    REC ("OASISTypes",
      [ ("obj_modules", of_list of_string v.obj_modules);
        ("obj_findlib_fullname",
         of_option (of_list of_string) v.obj_findlib_fullname);
        ("obj_findlib_directory",
         of_option odn_of_unix_filename v.obj_findlib_directory);
      ])
  in
  let odn_of_executable v =
    REC ("OASISTypes",
      [ ("exec_custom", (of_bool v.exec_custom));
        ("exec_main_is", (odn_of_unix_filename v.exec_main_is)) ])
  in
  let odn_of_flag v =
    REC ("OASISTypes",
      [ ("flag_description",
         (of_option of_string v.flag_description));
        ("flag_default", (odn_of_conditional of_bool v.flag_default)) ])
  in
  let odn_of_vcs =
    function
    | Darcs -> VRT ("OASISTypes.Darcs", [])
    | Git -> VRT ("OASISTypes.Git", [])
    | Svn -> VRT ("OASISTypes.Svn", [])
    | Cvs -> VRT ("OASISTypes.Cvs", [])
    | Hg -> VRT ("OASISTypes.Hg", [])
    | Bzr -> VRT ("OASISTypes.Bzr", [])
    | Arch -> VRT ("OASISTypes.Arch", [])
    | Monotone -> VRT ("OASISTypes.Monotone", [])
    | OtherVCS v0 -> VRT ("OASISTypes.OtherVCS", [ STR v0 ])
  in
  let odn_of_source_repository v =
    REC ("OASISTypes",
      [ ("src_repo_type", (odn_of_vcs v.src_repo_type));
        ("src_repo_location", (STR v.src_repo_location));
        ("src_repo_browser",
         (of_option of_string v.src_repo_browser));
        ("src_repo_module",
         (of_option of_string v.src_repo_module));
        ("src_repo_branch",
         (of_option of_string v.src_repo_branch));
        ("src_repo_tag",
         (of_option of_string v.src_repo_tag));
        ("src_repo_subdir",
         (of_option odn_of_unix_filename v.src_repo_subdir)) ])
  in
  let odn_of_test v =
    REC ("OASISTypes",
      [ ("test_type",
         (odn_of_plugin (function | `Test -> PVR ("Test", None))
            v.test_type));
        ("test_command",
         (odn_of_conditional odn_of_command_line v.test_command));
        ("test_custom", (odn_of_custom v.test_custom));
        ("test_working_directory",
         (of_option odn_of_unix_filename v.test_working_directory));
        ("test_run", (odn_of_conditional of_bool v.test_run));
        ("test_tools", (of_list odn_of_tool v.test_tools)) ])
  in
  let odn_of_doc_format =
    function
    | HTML v0 -> VRT ("OASISTypes.HTML", [ odn_of_unix_filename v0 ])
    | DocText -> VRT ("OASISTypes.DocText", [])
    | PDF -> VRT ("OASISTypes.PDF", [])
    | PostScript -> VRT ("OASISTypes.PostScript", [])
    | Info v0 -> VRT ("OASISTypes.Info", [ odn_of_unix_filename v0 ])
    | DVI -> VRT ("OASISTypes.DVI", [])
    | OtherDoc -> VRT ("OASISTypes.OtherDoc", [])
  in
  let odn_of_doc v =
    REC ("OASISTypes",
      [ ("doc_type",
         (odn_of_plugin (function | `Doc -> PVR ("Doc", None)) v.doc_type));
        ("doc_custom", (odn_of_custom v.doc_custom));
        ("doc_build", (odn_of_conditional of_bool v.doc_build));
        ("doc_install", (odn_of_conditional of_bool v.doc_install));
        ("doc_install_dir", (odn_of_unix_filename v.doc_install_dir));
        ("doc_title", (of_string v.doc_title));
        ("doc_authors", (of_list of_string v.doc_authors));
        ("doc_abstract",
         (of_option of_string v.doc_abstract));
        ("doc_format", (odn_of_doc_format v.doc_format));
        ("doc_data_files",
         (of_list
            (fun (v1, v0) ->
               TPL
                 [ odn_of_unix_filename v1;
                   of_option odn_of_unix_filename v0 ])
            v.doc_data_files));
        ("doc_build_tools",
         (of_list odn_of_tool v.doc_build_tools)) ])
  in
  let odn_of_section =
    function
    | Library ((v2, v1, v0)) ->
        VRT ("OASISTypes.Library",
          [ odn_of_common_section v2; odn_of_build_section v1;
            odn_of_library v0 ])
    | Object ((v2, v1, v0)) ->
        VRT ("OASISTypes.Object",
          [ odn_of_common_section v2; odn_of_build_section v1;
            odn_of_object_ v0 ])
    | Executable ((v2, v1, v0)) ->
        VRT ("OASISTypes.Executable",
          [ odn_of_common_section v2; odn_of_build_section v1;
            odn_of_executable v0 ])
    | Flag ((v1, v0)) ->
        VRT ("OASISTypes.Flag",
          [ odn_of_common_section v1; odn_of_flag v0 ])
    | SrcRepo ((v1, v0)) ->
        VRT ("OASISTypes.SrcRepo",
          [ odn_of_common_section v1; odn_of_source_repository v0 ])
    | Test ((v1, v0)) ->
        VRT ("OASISTypes.Test",
          [ odn_of_common_section v1; odn_of_test v0 ])
    | Doc ((v1, v0)) ->
        VRT ("OASISTypes.Doc", [ odn_of_common_section v1; odn_of_doc v0 ])
  in
  let odn_of_text =
    let odn_of_elt =
      function
      | OASISText.Para v0 -> VRT ("OASISText.Para", [ of_string v0 ])
      | OASISText.Verbatim v0 -> VRT ("OASISText.Verbatim", [ of_string v0 ])
      | OASISText.BlankLine -> VRT ("OASISText.BlankLine", [])
    in
    of_list odn_of_elt
  in
  REC ("OASISTypes",
    [ ("oasis_version", (OASISVersion.odn_of_t pkg.oasis_version));
      ("ocaml_version", (of_option OASISVersion.odn_of_comparator) pkg.ocaml_version);
      ("version",         (OASISVersion.odn_of_t pkg.version));
      ("license",         (OASISLicense.odn_of_t pkg.license));
      ("findlib_version", (of_option OASISVersion.odn_of_comparator pkg.findlib_version));
      ("alpha_features",  (of_list of_string pkg.alpha_features));
      ("beta_features",   (of_list of_string pkg.beta_features));
      ("name",            (STR pkg.name));
      ("license_file",    (of_option odn_of_unix_filename pkg.license_file));
      ("copyrights",      (of_list of_string pkg.copyrights));
      ("maintainers",     (of_list of_string pkg.maintainers));
      ("authors",         (of_list of_string pkg.authors));
      ("homepage",        (of_option of_string pkg.homepage));
      ("bugreports",      (of_option of_string pkg.bugreports));
      ("synopsis",        (of_string pkg.synopsis));
      ("description",     (of_option odn_of_text pkg.description));
      ("tags",            (of_list of_string pkg.tags));
      ("categories",      (of_list of_string pkg.categories));
      ("files_ab",        (of_list odn_of_unix_filename pkg.files_ab));
      ("sections",        (of_list odn_of_section pkg.sections));
      ("disable_oasis_section", (of_list odn_of_unix_filename pkg.disable_oasis_section));
      ("conf_type",
       (odn_of_plugin (function | `Configure -> PVR ("Configure", None))
          pkg.conf_type));
      ("conf_custom", (odn_of_custom pkg.conf_custom));
      ("build_type",
       (odn_of_plugin (function | `Build -> PVR ("Build", None))
          pkg.build_type));
      ("build_custom", (odn_of_custom pkg.build_custom));
      ("install_type",
       (odn_of_plugin (function | `Install -> PVR ("Install", None))
          pkg.install_type));
      ("install_custom", (odn_of_custom pkg.install_custom));
      ("uninstall_custom", (odn_of_custom pkg.uninstall_custom));
      ("clean_custom", (odn_of_custom pkg.clean_custom));
      ("distclean_custom", (odn_of_custom pkg.distclean_custom));
      ("plugins",
       ((fun x ->
           of_list
             (odn_of_plugin (function | `Extra -> PVR ("Extra", None))) x)
          pkg.plugins));
      ("schema_data", proplist_data);
      ("plugin_data", LST []);
    ])
