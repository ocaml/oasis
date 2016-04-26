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


open OASISGettext
open OASISTypes
open OASISExpr
open BaseCheck
open BaseEnv


let ocamlfind  = BaseCheck.ocamlfind
let ocamlc     = BaseOCamlcConfig.ocamlc
let ocamlopt   = prog_opt "ocamlopt"
let ocamlbuild = prog "ocamlbuild"


(**/**)
let rpkg =
  ref None


let pkg_get () =
  match !rpkg with
    | Some pkg -> pkg
    | None -> failwith (s_ "OASIS Package is not set")


let var_cond = ref []


let var_define_cond ~since_version f dflt =
  let holder = ref (fun () -> dflt) in
  let since_version =
    OASISVersion.VGreaterEqual (OASISVersion.version_of_string since_version)
  in
  var_cond :=
    (fun ver ->
       if OASISVersion.comparator_apply ver since_version then
         holder := f ()) :: !var_cond;
  fun () -> !holder ()


(**/**)


let pkg_name =
  var_define
    ~short_desc:(fun () -> s_ "Package name")
    "pkg_name"
    (fun () -> (pkg_get ()).name)


let pkg_version =
  var_define
    ~short_desc:(fun () -> s_ "Package version")
    "pkg_version"
    (fun () ->
       (OASISVersion.string_of_version (pkg_get ()).version))


let c = BaseOCamlcConfig.var_define


let os_type        = c "os_type"
let system         = c "system"
let architecture   = c "architecture"
let ccomp_type     = c "ccomp_type"
let ocaml_version  = c "ocaml_version"


(* TODO: Check standard variable presence at runtime *)


let standard_library_default = c "standard_library_default"
let standard_library         = c "standard_library"
let standard_runtime         = c "standard_runtime"
let bytecomp_c_compiler      = c "bytecomp_c_compiler"
let native_c_compiler        = c "native_c_compiler"
let model                    = c "model"
let ext_obj                  = c "ext_obj"
let ext_asm                  = c "ext_asm"
let ext_lib                  = c "ext_lib"
let ext_dll                  = c "ext_dll"
let default_executable_name  = c "default_executable_name"
let systhread_supported      = c "systhread_supported"


let flexlink =
  BaseCheck.prog "flexlink"


let flexdll_version =
  var_define
    ~short_desc:(fun () -> "FlexDLL version (Win32)")
    "flexdll_version"
    (fun () ->
       let lst =
         OASISExec.run_read_output ~ctxt:!BaseContext.default
           (flexlink ()) ["-help"]
       in
       match lst with
         | line :: _ ->
           Scanf.sscanf line "FlexDLL version %s" (fun ver -> ver)
         | [] ->
           raise Not_found)


(**/**)
let p name hlp dflt =
  var_define
    ~short_desc:hlp
    ~cli:CLIAuto
    ~arg_help:"dir"
    name
    dflt


let (/) a b =
  if os_type () = Sys.os_type then
    Filename.concat a b
  else if os_type () = "Unix" then
    OASISUnixPath.concat a b
  else
    OASISUtils.failwithf (f_ "Cannot handle os_type %s filename concat")
      (os_type ())
(**/**)


let prefix =
  p "prefix"
    (fun () -> s_ "Install architecture-independent files dir")
    (fun () ->
       match os_type () with
         | "Win32" ->
           let program_files =
             Sys.getenv "PROGRAMFILES"
           in
           program_files/(pkg_name ())
         | _ ->
           "/usr/local")


let exec_prefix =
  p "exec_prefix"
    (fun () -> s_ "Install architecture-dependent files in dir")
    (fun () -> "$prefix")


let bindir =
  p "bindir"
    (fun () -> s_ "User executables")
    (fun () -> "$exec_prefix"/"bin")


let sbindir =
  p "sbindir"
    (fun () -> s_ "System admin executables")
    (fun () -> "$exec_prefix"/"sbin")


let libexecdir =
  p "libexecdir"
    (fun () -> s_ "Program executables")
    (fun () -> "$exec_prefix"/"libexec")


let sysconfdir =
  p "sysconfdir"
    (fun () -> s_ "Read-only single-machine data")
    (fun () -> "$prefix"/"etc")


let sharedstatedir =
  p "sharedstatedir"
    (fun () -> s_ "Modifiable architecture-independent data")
    (fun () -> "$prefix"/"com")


let localstatedir =
  p "localstatedir"
    (fun () -> s_ "Modifiable single-machine data")
    (fun () -> "$prefix"/"var")


let libdir =
  p "libdir"
    (fun () -> s_ "Object code libraries")
    (fun () -> "$exec_prefix"/"lib")


let datarootdir =
  p "datarootdir"
    (fun () -> s_ "Read-only arch-independent data root")
    (fun () -> "$prefix"/"share")


let datadir =
  p "datadir"
    (fun () -> s_ "Read-only architecture-independent data")
    (fun () -> "$datarootdir")


let infodir =
  p "infodir"
    (fun () -> s_ "Info documentation")
    (fun () -> "$datarootdir"/"info")


let localedir =
  p "localedir"
    (fun () -> s_ "Locale-dependent data")
    (fun () -> "$datarootdir"/"locale")


let mandir =
  p "mandir"
    (fun () -> s_ "Man documentation")
    (fun () -> "$datarootdir"/"man")


let docdir =
  p "docdir"
    (fun () -> s_ "Documentation root")
    (fun () -> "$datarootdir"/"doc"/"$pkg_name")


let htmldir =
  p "htmldir"
    (fun () -> s_ "HTML documentation")
    (fun () -> "$docdir")


let dvidir =
  p "dvidir"
    (fun () -> s_ "DVI documentation")
    (fun () -> "$docdir")


let pdfdir =
  p "pdfdir"
    (fun () -> s_ "PDF documentation")
    (fun () -> "$docdir")


let psdir =
  p "psdir"
    (fun () -> s_ "PS documentation")
    (fun () -> "$docdir")


let destdir =
  p "destdir"
    (fun () -> s_ "Prepend a path when installing package")
    (fun () ->
       raise
         (PropList.Not_set
            ("destdir",
             Some (s_ "undefined by construct"))))


let findlib_version =
  var_define
    "findlib_version"
    (fun () ->
       BaseCheck.package_version "findlib")


let is_native =
  var_define
    "is_native"
    (fun () ->
       try
         let _s: string =
           ocamlopt ()
         in
         "true"
       with PropList.Not_set _ ->
         let _s: string =
           ocamlc ()
         in
         "false")


let ext_program =
  var_define
    "suffix_program"
    (fun () ->
       match os_type () with
         | "Win32" | "Cygwin" -> ".exe"
         | _ -> "")


let rm =
  var_define
    ~short_desc:(fun () -> s_ "Remove a file.")
    "rm"
    (fun () ->
       match os_type () with
         | "Win32" -> "del"
         | _ -> "rm -f")


let rmdir =
  var_define
    ~short_desc:(fun () -> s_ "Remove a directory.")
    "rmdir"
    (fun () ->
       match os_type () with
         | "Win32" -> "rd"
         | _ -> "rm -rf")


let debug =
  var_define
    ~short_desc:(fun () -> s_ "Turn ocaml debug flag on")
    ~cli:CLIEnable
    "debug"
    (fun () -> "true")


let profile =
  var_define
    ~short_desc:(fun () -> s_ "Turn ocaml profile flag on")
    ~cli:CLIEnable
    "profile"
    (fun () -> "false")


let tests =
  var_define_cond ~since_version:"0.3"
    (fun () ->
       var_define
         ~short_desc:(fun () ->
           s_ "Compile tests executable and library and run them")
         ~cli:CLIEnable
         "tests"
         (fun () -> "false"))
    "true"


let docs =
  var_define_cond ~since_version:"0.3"
    (fun () ->
       var_define
         ~short_desc:(fun () -> s_ "Create documentations")
         ~cli:CLIEnable
         "docs"
         (fun () -> "true"))
    "true"


let native_dynlink =
  var_define
    ~short_desc:(fun () -> s_ "Compiler support generation of .cmxs.")
    ~cli:CLINone
    "native_dynlink"
    (fun () ->
       let res =
         let ocaml_lt_312 () =
           OASISVersion.comparator_apply
             (OASISVersion.version_of_string (ocaml_version ()))
             (OASISVersion.VLesser
                (OASISVersion.version_of_string "3.12.0"))
         in
         let flexdll_lt_030 () =
           OASISVersion.comparator_apply
             (OASISVersion.version_of_string (flexdll_version ()))
             (OASISVersion.VLesser
                (OASISVersion.version_of_string "0.30"))
         in
         let has_native_dynlink =
           let ocamlfind = ocamlfind () in
           try
             let fn =
               OASISExec.run_read_one_line
                 ~ctxt:!BaseContext.default
                 ocamlfind
                 ["query"; "-predicates"; "native"; "dynlink";
                  "-format"; "%d/%a"]
             in
             Sys.file_exists fn
           with _ ->
             false
         in
         if not has_native_dynlink then
           false
         else if ocaml_lt_312 () then
           false
         else if (os_type () = "Win32" || os_type () = "Cygwin")
              && flexdll_lt_030 () then
           begin
             BaseMessage.warning
               (f_ ".cmxs generation disabled because FlexDLL needs to be \
                    at least 0.30. Please upgrade FlexDLL from %s to 0.30.")
               (flexdll_version ());
             false
           end
         else
           true
       in
       string_of_bool res)


let init pkg =
  rpkg := Some pkg;
  List.iter (fun f -> f pkg.oasis_version) !var_cond

