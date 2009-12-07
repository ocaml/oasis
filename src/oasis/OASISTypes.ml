
(** OASIS types and exceptions
   @author Sylvain Le Gall
  *)

(** Alias type
  *)
type name               = string;;
type package_name       = string;;
type url                = string;;
type version            = string;;
type version_constraint = string;;
type dirname            = string;;
type filename           = string;;
type prog               = string;;

(** A mandatory field is not defined *)
exception MissingField of name list;;
(** Unable to recognize one field *)
exception UnknownField of name;;

(** Valid licenses
  *)
type license =
    [ `AllRightsReserved
    | `BSD3
    | `BSD4
    | `GPL
    | `LGPL
    | `LGPL_link_exn
    | `Other of url
    | `PublicDomain ]
;;

(** Compilation type
  *)
type compiled_object =
  | Byte
  | Native
  | Best
;;

(** Package dependency
  *)
type dependency = 
  | FindlibPackage of package_name * version_constraint option
  | InternalLibrary of name
;;

(** Possible VCS 
  *)
type vcs_t = 
  | Darcs 
  | Git 
  | Svn 
  | Cvs 
  | Hg 
  | Bzr 
  | Arch 
  | Monotone
;;

(** Available test 
  *)
type test = 
  | TOs_type
  | TSystem
  | TArchitecture
  | TCcomp_type
  | TOCaml_version
;;

(** Boolean expression to express condition on values
  *)
type expr =
  | EBool of bool
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EFlag of string
  | ETest of test * string
;;

(** Conditional value
  *)
type 'a conditional =
    (expr * 'a) list
;;

(** Library definition 
  *)
type 'a library = {
  lib_build:           bool conditional;
  lib_install:         bool conditional;
  lib_path:            dirname;
  lib_modules:         string list;
  lib_compiled_object: compiled_object;
  lib_build_depends:   dependency list;
  lib_build_tools:     prog list;
  lib_c_sources:       filename list;
  lib_data_files:      (filename * filename) list;
  lib_schema_data:     'a;
}
;;

(** Executable definition 
  *)
type 'a executable = {
  exec_build:           bool conditional;
  exec_install:         bool conditional;
  exec_main_is:         filename;
  exec_compiled_object: compiled_object;
  exec_build_depends:   dependency list;
  exec_build_tools:     prog list;
  exec_c_sources:       filename list;
  exec_custom:          bool;
  exec_data_files:      (filename * filename) list;
  exec_is:              filename; (* Real executable *)
  exec_schema_data:     'a;
}
;;

(** Command line flag defintion 
  *)
type 'a flag = {
  flag_description:  string option;
  flag_default:      bool conditional;
  flag_schema_data:  'a;
}
;;

(** Source repository definition
  *)
type 'a source_repository = {
  src_repo_type:      vcs_t;
  src_repo_location:  url;
  src_repo_browser:   url option;
  src_repo_module:    string option;
  src_repo_branch:    string option;
  src_repo_tag:       string option;
  src_repo_subdir:    filename option;
}
;;

(** Test definition
  *)
type 'a test_t = {
  test_type:               string;
  test_command:            string;
  test_working_directory:  filename option;
  test_run:                bool conditional;
  test_build_tools:        prog list;
  test_schema_data:        'a;
}
;;

(** OASIS file whole content
  *)
type 'a package = {
  oasis_version:  version;
  ocaml_version:  version_constraint option;
  name:           package_name;
  version:        version;
  license:        license;
  license_file:   filename;
  copyrights:     string list;
  maintainers:    string list;
  authors:        string list;
  homepage:       url option;
  synopsis:       string;
  description:    string option;
  categories:     url list;
  build_depends:  dependency list;
  build_tools:    prog list;
  conf_type:      string;
  build_type:     string;
  install_type:   string;
  files_ab:       filename list;
  plugins:        string list;
  libraries:      (name * 'a library) list;
  executables:    (name * 'a executable) list;
  flags:          (name * 'a flag) list;
  src_repos:      (name * 'a source_repository) list;
  tests:          (name * 'a test_t) list;
  schema_data:    'a;
}
;;

