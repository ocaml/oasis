
(** OASIS types and exceptions
   @author Sylvain Le Gall
  *)

TYPE_CONV_PATH "OASISTypes"

(** Alias type
  *)
type name         = string with odn
type package_name = string with odn
type url          = string with odn
type dirname      = string with odn
type filename     = string with odn
type prog         = string with odn
type arg          = string with odn
type args         = arg list with odn
type command      = string with odn
type command_line = (command * args) with odn

(* Package name for findlib, doesn't contain '.' *)
type findlib_name = string with odn 
(* Package path, made of several findlib name concatenated with '.' *)
type findlib_path = string with odn

(** Version 
  *)
type version =
  | VInt of int * version
  | VNonInt of string * version
  | VEnd
  with odn

(** Version comparator
  *)
type version_comparator = 
  | VGreater of version
  | VGreaterEqual of version
  | VEqual of version
  | VLesser of version
  | VLesserEqual of version
  | VOr of  version_comparator * version_comparator
  | VAnd of version_comparator * version_comparator
  with odn

(** Valid licenses
  *)
type license =
  | AllRightsReserved
  | BSD3
  | BSD4
  | GPL
  | LGPL
  | LGPL_link_exn
  | PublicDomain
  | OtherLicense of url
  with odn

(** Compilation type
  *)
type compiled_object =
  | Byte
  | Native
  | Best
  with odn

(** Package dependency
  *)
type dependency = 
  | FindlibPackage of findlib_path * version_comparator option
  | InternalLibrary of name
  with odn

(** Tool dependency
  *)
type tool =
  | ExternalTool of name
  | InternalExecutable of name 
  with odn

(** Possible VCS 
  *)
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

(** Available test 
  *)
type expr_test = 
  | TOs_type
  | TSystem
  | TArchitecture
  | TCcomp_type
  | TOCaml_version
  with odn

(** Boolean expression to express condition on values
  *)
type expr =
  | EBool of bool
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EFlag of string
  | ETest of expr_test * string
  with odn

(** Conditional value
  *)
type 'a conditional = (expr * 'a) list with odn

type plugin = name * version option with odn

type common_section =
    {
      cs_name: name;
      cs_data: PropList.Data.t;
    }
    with odn

type build_section =
    {
      bs_build:           bool conditional;
      bs_install:         bool conditional;
      bs_path:            dirname;
      bs_compiled_object: compiled_object;
      bs_build_depends:   dependency list;
      bs_build_tools:     tool list;
      bs_c_sources:       filename list;
      bs_data_files:      (filename * filename option) list;
      bs_ccopt:           args conditional;
      bs_cclib:           args conditional;
      bs_dlllib:          args conditional;
      bs_dllpath:         args conditional;
      bs_byteopt:         args conditional;
      bs_nativeopt:       args conditional;
    }
    with odn

(** Library definition 
  *)
type library = 
    {
      lib_modules:            string list;
      lib_findlib_parent:     findlib_name option;
      lib_findlib_name:       findlib_name option;
      lib_findlib_containers: findlib_name list;
    } with odn

(** Executable definition 
  *)
type executable = 
    {
      exec_custom:          bool;
      exec_main_is:         filename;
    } with odn

(** Command line flag defintion 
  *)
type flag = 
    {
      flag_description:  string option;
      flag_default:      bool conditional;
    } with odn

(** Source repository definition
  *)
type source_repository = 
    {
      src_repo_type:        vcs;
      src_repo_location:    url;
      src_repo_browser:     url option;
      src_repo_module:      string option;
      src_repo_branch:      string option;
      src_repo_tag:         string option;
      src_repo_subdir:      filename option;
    } with odn

(** Test definition
  *)
type test = 
    {
      test_type:               plugin;
      test_command:            command_line conditional;
      test_working_directory:  filename option;
      test_run:                bool conditional;
      test_build_tools:        tool list;
    } with odn

type section =
  | Library    of common_section * build_section * library
  | Executable of common_section * build_section * executable
  | Flag       of common_section * flag
  | SrcRepo    of common_section * source_repository
  | Test       of common_section * test
  with odn

(** OASIS file whole content
  *)
type package = 
    {
      oasis_version:   version;
      ocaml_version:   version_comparator option;
      findlib_version: version_comparator option;
      name:            package_name;
      version:         version;
      license:         license;
      license_file:    filename option;
      copyrights:      string list;
      maintainers:     string list;
      authors:         string list;
      homepage:        url option;
      synopsis:        string;
      description:     string option;
      categories:      url list;
      conf_type:       plugin;
      build_type:      plugin;
      install_type:    plugin;
      files_ab:        filename list;
      sections:        section list;
      plugins:         plugin list;
      schema_data:     PropList.Data.t;
    } with odn

(* END EXPORT *)

(** Configuration for parsing and checking 
  *)
type conf =
  {
    oasisfn:        filename option;
    srcdir:         dirname option;
    debug:          bool;
    ignore_unknown: bool;
  }

(** Definition of a value in OASIS file
  *)
type 'a value =
    {
      parse:  string -> 'a;
      update: 'a -> 'a -> 'a;
      print:  'a -> string;
    }

(** Quickstart level
  *)
type 'a quickstart_level =
  | NoChoice of 'a (* Don't ask question, use provided value *)
  | Beginner
  | Intermediate
  | Expert

(** Howto ask questions in quickstart 
  *)
type 'a quickstart_question =
  | YesNo
  | Field
  | Text
  | Choices of 'a list
  | ExclusiveChoices of 'a list
