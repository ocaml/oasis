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


(** Package definition

    This module contains all the types used to build an OASIS package.
    A package is described by an '_oasis' file.

    @author Sylvain Le Gall
*)


(** {2 Aliases} *)


type name          = string  (** Standard name. *)
type package_name  = string  (** Name of a package, see {!package}. *)
type url           = string  (** Standard URL. *)
type unix_dirname  = string  (** UNIX directory name. *)
type unix_filename = string  (** UNIX file name. *)
type host_dirname  = string  (** Host directory name. *)
type host_filename = string  (** Host file name. *)
type prog          = string  (** Program. *)
type arg           = string  (** Command line argument. *)
type args          = arg list (** Command line arguments. *)
type command_line  = (prog * args) (** Command line. *)


(** Package name for findlib, doesn't contain '.'. *)
type findlib_name = string


(** Package name + path for findlib, made of several findlib name concatenated
    with '.'. *)
type findlib_full = string


(** {2 OASIS package definition} *)


(** Compilation type.
*)
type compiled_object =
  | Byte   (** Always use byte code. *)
  | Native (** Always use native code. *)
  | Best   (** Use either byte or native, depending ocamlopt availability. *)


(** Package dependency.
*)
type dependency =
  | FindlibPackage of findlib_full * OASISVersion.comparator option
  (** External findlib package. *)

  | InternalLibrary of name
  (** Section library of the given name. *)


(** Tool dependency.
*)
type tool =
  | ExternalTool of name
  (** External tool. *)

  | InternalExecutable of name
  (** Section executable of the given name. *)


(** Possible VCS.
*)
type vcs =
  | Darcs (** Darcs *)
  | Git   (** Git *)
  | Svn   (** Subversion *)
  | Cvs   (** CVS *)
  | Hg    (** Mercurial *)
  | Bzr   (** Bazaar *)
  | Arch  (** GNU arch *)
  | Monotone (** Monotone *)
  | OtherVCS of url (** Anything else, follow URL for description. *)


(** Conditional value, last expression that evaluate to true define
    the value.
*)
type 'a conditional = 'a OASISExpr.choices


(** Plugin kind.
*)
type plugin_kind =
  [`Configure
  | `Build
  | `Doc
  | `Test
  | `Install
  | `Extra]


(** Additional data to allow registration of more than
    one data property per plugin. See {!OASISPlugin.data_new_property}
*)
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


(** Plugin definition, plugin type depends on which fields this
    types is used for.
*)
type 'a plugin = 'a * name * OASISVersion.t option


type all_plugin = plugin_kind plugin


(** Property list storage for plugin data
*)
type plugin_data = (all_plugin * plugin_data_purpose * (unit -> unit)) list


(** Set of command lines to run before and after a step.
*)
type custom =
  {
    pre_command:  (command_line option) conditional;
    (** Run before. *)

    post_command: (command_line option) conditional;
    (** Run after. *)
  }


(** Common attributes for sections.
*)
type common_section =
  {
    cs_name: name;
    (** Name of the section. *)

    (* TODO: get rid of schema_data and cs_data *)
    cs_data: PropList.Data.t;
    cs_plugin_data: plugin_data;
    (** Property list attached to the section. *)
  }


(** Common attributes for Executable and Library sections.
*)
type build_section =
  {
    bs_build:           bool conditional;
    (** Build this section. *)
    bs_install:         bool conditional;
    (** Install this section. *)
    bs_path:            unix_dirname;
    (** Top level location of the sources. *)
    bs_compiled_object: compiled_object;
    (** What kind of compilation. *)
    bs_build_depends:   dependency list;
    (** List of dependencies. *)
    bs_build_tools:     tool list;
    (** List of build tools. *)
    bs_c_sources:       unix_filename list;
    (** C sources, relative to [bs_path]. *)
    bs_data_files:      (unix_filename * unix_filename option) list;
    (** Data files and their install location, relative to [bs_path]. *)
    bs_ccopt:           args conditional;
    (** Options for -ccopt. *)
    bs_cclib:           args conditional;
    (** Options for -cclib. *)
    bs_dlllib:          args conditional;
    (** Options for -dllib. *)
    bs_dllpath:         args conditional;
    (** Options for -dllpath. *)
    bs_byteopt:         args conditional;
    (** Option to pass to ocamlc. *)
    bs_nativeopt:       args conditional;
    (** Option to pass to ocamlopt. *)
  }


(** Library definition.
*)
type library =
  {
    lib_modules:            string list;
    (** List of modules exported by the library. *)
    lib_pack:               bool;
    (** Are we building a packed library? *)
    lib_internal_modules:   string list;
    (** List of modules not-exported by the library, but compiled along. *)
    lib_findlib_parent:     findlib_name option;
    (** Name of the findlib parent, if any. *)
    lib_findlib_name:       findlib_name option;
    (** Findlib name of this library, this name is used to refer to this
        library in build dependencies.
    *)
    lib_findlib_containers: findlib_name list;
    (** Name of virtual containers (empty findlib package) between findlib
        parent and findlib name
    *)
  }


(** Object definition.
*)
type object_ =
  {
    obj_modules:            string list;
    obj_findlib_fullname:   findlib_name list option;
    (** Findlib name of this library, this name is used to refer to this
        library in build dependencies.
    *)
  }


(** Executable definition.
*)
type executable =
  {
    exec_custom:          bool;
    (** Use -custom ocamlc option. *)
    exec_main_is:         unix_filename;
    (** Main file to compile, dependencies should be guessed
        by build system.
    *)
  }


(** Command line flag definition.
*)
type flag =
  {
    flag_description:  string option;
    (** Short description. *)
    flag_default:      bool conditional;
    (** Default value. *)
  }


(** Source repository definition.
*)
type source_repository =
  {
    src_repo_type:        vcs;
    (** Repository type. *)
    src_repo_location:    url;
    (** Where to fetch the source, using appropriate VCS tool. *)
    src_repo_browser:     url option;
    (** Where to browse the source, using web browser. *)
    src_repo_module:      string option;
    (** Depending on VCS, which module (e.g. CVS). *)
    src_repo_branch:      string option;
    (** Depending on VCS, which branch (e.g. git). *)
    src_repo_tag:         string option;
    (** Depending on VCS, which tag (e.g. tag for darcs, git or svn). *)
    src_repo_subdir:      unix_filename option;
    (** Depending on VCS, which sub directory (e.g. svn). *)
  }


(** Test definition.
*)
type test =
  {
    test_type:               [`Test] plugin;
    (** Plugin to run the test, default custom. *)
    test_command:            command_line conditional;
    (** Command to run the test, may depend on the plugin meaning. *)
    test_custom:             custom;
    (** Custom command lines to run before and after. *)
    test_working_directory:  unix_filename option;
    (** Which working directorty to chdir in. *)
    test_run:                bool conditional;
    (** Should we run the test. *)
    test_tools:              tool list;
    (** Tools required for this test. *)
  }


(** Document formats.
*)
type doc_format =
  | HTML of unix_filename (** HTML files and their main entry point
                              (e.g. [HTML "index.html"])
                          *)
  | DocText               (** Plain text. *)
  | PDF                   (** Portable document format. *)
  | PostScript
  | Info of unix_filename (** Info files and their main entry point. *)
  | DVI
  | OtherDoc              (** Anything else. *)


(** Document definition.
*)
type doc =
  {
    doc_type:        [`Doc] plugin;
    (** Plugin to build this document, default none. *)
    doc_custom:      custom;
    (** Custom command lines ommand to before and after. *)
    doc_build:       bool conditional;
    (** Build this document. *)
    doc_install:     bool conditional;
    (** Install this document. *)
    doc_install_dir: unix_filename;
    (** Where to install this document. *)
    doc_title:       string;
    (** What is the title of this document. *)
    doc_authors:     string list;
    (** Who are the authors of this document. *)
    doc_abstract:    string option;
    (** Abstract of this document. *)
    doc_format:      doc_format;
    (** Format of this document. *)
    doc_data_files:  (unix_filename * unix_filename option) list;
    (** All data files and where they should be install, by default
        to doc_install_dir
    *)
    doc_build_tools: tool list;
    (** Tools required to generate this document. *)
  }


(** All sections and their attributes. *)
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


(** OASIS package, what an '_oasis' file contains.
*)
type package =
  {
    oasis_version:          OASISVersion.t;
    (** OASIS version used to write this package. *)
    ocaml_version:          OASISVersion.comparator option;
    (** OCaml version required for this package. *)
    findlib_version:        OASISVersion.comparator option;
    (** Findlib version required for this package. *)
    alpha_features:         string list;
    (** Alpha features enabled for this package. *)
    beta_features:          string list;
    (** Beta features enabled for this package. *)
    name:                   package_name;
    (** Name of this package. *)
    version:                OASISVersion.t;
    (** Version of this package. *)
    license:                OASISLicense.t;
    (** License of this package. *)
    license_file:           unix_filename option;
    (** File containing the license of this package. *)
    copyrights:             string list;
    (** Copyright holders (e.g. companies or people). *)
    maintainers:            string list;
    (** People actually taking care of this package (e.g. subset of copyright
        holders)
    *)
    authors:                string list;
    (** Real people who wrote this package, not their companies. *)
    homepage:               url option;
    (** Location of the package homepage. *)
    bugreports:             url option;
    (** Location of the page to report bugs. *)
    synopsis:               string;
    (** Short description of the package. *)
    description:            OASISText.t option;
    (** Long description of the package. *)
    tags:                   string list;
    (** List of tags. *)
    categories:             url list;
    (** List of categories that the package belong to. *)

    conf_type:              [`Configure] plugin;
    (** Plugin to configure, default internal. *)
    conf_custom:            custom;
    (** Actions around configure step. *)

    build_type:             [`Build] plugin;
    (** Plugin to build, default ocamlbuild. *)
    build_custom:           custom;
    (** Actions around build step. *)

    install_type:           [`Install] plugin;
    (** Plugin to install/uninstall, default internal. *)
    install_custom:         custom;
    (** Actions around install step. *)
    uninstall_custom:       custom;
    (** Actions around uninstall step. *)

    clean_custom:           custom;
    (** Actions around clean step. *)
    distclean_custom:       custom;
    (** Actions aroudn distclean step. *)

    files_ab:               unix_filename list;
    (** Files to generate by replacing token in it after configure step. *)
    sections:               section list;
    (** All sections (libraries, executables, tests...). *)
    plugins:                [`Extra] plugin list;
    (** Extra plugins applied. *)

    disable_oasis_section:  unix_filename list;
    (** Files which should not have OASIS Section comments and digests *)

    (* TODO: get rid of schema_data and cs_data *)
    schema_data:            PropList.Data.t;
    plugin_data:            plugin_data;
    (** Property list attached to this package. *)
  }


(** {2 Quickstart questions } *)


(** Quickstart level. {b Not exported}.
*)
type 'a quickstart_level =
  | NoChoice of 'a (** Don't ask question, use provided value. *)
  | Beginner       (** Ask the question to a beginner and above. *)
  | Intermediate   (** Ask the question to an intermediate user and above. *)
  | Expert         (** Ask the question to an expert. *)


(** Howto ask questions in quickstart. {b Not exported}.
*)
type 'a quickstart_question =
  | Field   (** Short text. *)
  | Text    (** Long text, may use editor for it. *)
  | Choices of 'a list (** Multiple choices in a list. *)
  | ExclusiveChoices of 'a list (** Pick a single choice in the list. *)

