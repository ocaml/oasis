
(** OASIS types and exceptions
   @author Sylvain Le Gall
  *)

(** Alias type
  *)
type name         = string;;
type package_name = string;;
type url          = string;;
type version      = string;;
type dirname      = string;;
type filename     = string;;
type test         = string;;
type prog         = string;;

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
type dependency = package_name * version option
;;

(** Boolean expression to express condition on values
  *)
type expr =
  | EBool of bool
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EFlag of string
  | ETest of string * string
;;

(** Conditional value
  *)
type 'a conditional =
    (expr * 'a) list
;;

(** Library definition 
  *)
type library = {
  lib_buildable:       bool conditional;
  lib_installable:     bool conditional;
  lib_path:            dirname;
  lib_modules:         string list;
  lib_compiled_object: compiled_object;
  lib_extra:           (name * string) list;
}
;;

(** Executable definition 
  *)
type executable = {
  exec_buildable:       bool conditional;
  exec_installable:     bool conditional;
  exec_main_is:         filename;
  exec_compiled_object: compiled_object;
  exec_extra:           (name * string) list;
}
;;

(** Command line flag defintion 
  *)
type flag = {
  flag_description:  string option;
  flag_default:      bool conditional;
  flag_extra:        (name * string) list;
}
;;

(** OASIS file whole content
  *)
type package = {
  name:           package_name;
  version:        version;
  license:        license;
  license_file:   filename;
  copyright:      string option;
  maintainer:     string option;
  author:         string;
  homepage:       url option;
  synopsis:       string;
  description:    string option;
  categories:     url list;
  build_depends:  dependency list;
  build_tools:    prog list;
  conf_type:      string;
  build_type:     string;
  doc_type:       string;
  test_type:      string;
  install_type:   string;
  files_ab:       filename list;
  libraries:      (name * library) list;
  executables:    (name * executable) list;
  flags:          (name * flag) list;
  extra:          (name * string) list;
}
;;

