(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Most standard variables for OCaml 
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISTypes
open BaseCheck
open BaseEnv

(** {2 Programs} *)

let ocamlfind  = BaseCheck.ocamlfind
let ocamlc     = BaseOCamlcConfig.ocamlc
let ocamlopt   = prog_opt "ocamlopt"
let ocamlbuild = prog "ocamlbuild"

(** {2 Variables from OASIS package} 
  *)

(**/**)
let rpkg = 
  ref None

let pkg_get () =
  match !rpkg with 
    | Some pkg -> pkg
    | None -> failwith (s_ "OASIS Package is not set")
(**/**)

let pkg_name = 
  var_define
    ~short_desc:(fun () -> s_ "Package name")
    "pkg_name"
    (lazy (fst (pkg_get ())))

let pkg_version =
  var_define
    ~short_desc:(fun () -> s_ "Package version")
    "pkg_version"
    (lazy 
       (OASISVersion.string_of_version 
          (snd (pkg_get ()))))


(** {2 OCaml config variable} *) 

let c = BaseOCamlcConfig.var_define 
let stdc et = BaseOCamlcConfig.var_define (OASISExpr.string_of_expr_test et)

let os_type        = stdc TOs_type
let system         = stdc TSystem
let architecture   = stdc TArchitecture
let ccomp_type     = stdc TCcomp_type
let ocaml_version  = stdc TOCaml_version

(* Check variable presence *)
let () = 
  if false then 
    let v_of_et =
      function
        | TOs_type       -> os_type        
        | TSystem        -> system         
        | TArchitecture  -> architecture   
        | TCcomp_type    -> ccomp_type     
        | TOCaml_version -> ocaml_version  
    in
    let _lst : 'a list =
      List.map v_of_et OASISExpr.expr_tests
    in 
      ()

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


(** {2 Paths} *)

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
    BaseFilePath.Unix.concat a b 
  else
    OASISUtils.failwithf1 
      (f_ "Cannot handle os_type %s filename concat")
      (os_type ())
(**/**)

let prefix = 
  p "prefix"
    (fun () -> s_ "Install architecture-independent files dir")
    (lazy 
       (match os_type () with
          | "Win32" ->
              let program_files =
                Sys.getenv "PROGRAMFILES"
              in
                program_files/(pkg_name ())
          | _ ->
              "/usr/local"))

let exec_prefix = 
  p "exec_prefix"
    (fun () -> s_ "Install architecture-dependent files in dir")
    (lazy "$prefix")

let bindir =
  p "bindir"
    (fun () -> s_ "User executables")
    (lazy ("$exec_prefix"/"bin"))

let sbindir =
  p "sbindir"
    (fun () -> s_ "System admin executables")
    (lazy ("$exec_prefix"/"sbin"))

let libexecdir =
  p "libexecdir"
    (fun () -> s_ "Program executables")
    (lazy ("$exec_prefix"/"libexec"))

let sysconfdir =
  p "sysconfdir"
    (fun () -> s_ "Read-only single-machine data")
    (lazy ("$prefix"/"etc"))

let sharedstatedir =
  p "sharedstatedir"
    (fun () -> s_ "Modifiable architecture-independent data")
    (lazy ("$prefix"/"com"))

let localstatedir =
  p "localstatedir"
    (fun () -> s_ "Modifiable single-machine data")
    (lazy ("$prefix"/"var"))

let libdir =
  p "libdir"
    (fun () -> s_ "Object code libraries")
    (lazy ("$exec_prefix"/"lib"))

let datarootdir =
  p "datarootdir"
    (fun () -> s_ "Read-only arch-independent data root")
    (lazy ("$prefix"/"share"))

let datadir =
  p "datadir"
    (fun () -> s_ "Read-only architecture-independent data")
    (lazy ("$datarootdir"))

let infodir =
  p "infodir"
    (fun () -> s_ "Info documentation")
    (lazy ("$datarootdir"/"info"))

let localedir =
  p "localedir"
    (fun () -> s_ "Locale-dependent data")
    (lazy ("$datarootdir"/"locale"))

let mandir =
  p "mandir"
    (fun () -> s_ "Man documentation")
    (lazy ("$datarootdir"/"man"))

let docdir =
  p "docdir"
    (fun () -> s_ "Documentation root")
    (lazy ("$datarootdir"/"doc"/"$pkg_name"))

let htmldir =
  p "htmldir"
    (fun () -> s_ "HTML documentation")
    (lazy ("$docdir"))

let dvidir =
  p "dvidir"
    (fun () -> s_ "DVI documentation")
    (lazy ("$docdir"))

let pdfdir =
  p "pdfdir"
    (fun () -> s_ "PDF documentation")
    (lazy ("$docdir"))

let psdir =
  p "psdir"
    (fun () -> s_ "PS documentation")
    (lazy ("$docdir"))

let destdir =
  p "destdir"
    (fun () -> s_ "Prepend a path when installing package")
    (lazy 
       (raise 
          (PropList.Not_set
             ("destdir", 
              Some (s_ "undefined by construct")))))

(** {2 ...} *)

(** Findlib version
  *)
let findlib_version =
  var_define
    "findlib_version"
    (lazy 
       (BaseCheck.package_version "findlib"))

(** Check that the platform is a native platform (can compile native
    exec/library).
  *)
let is_native =
  var_define
    "is_native"
    (lazy
       (try
          let _s : string = 
            ocamlopt ()
          in
            "true"
        with PropList.Not_set _ ->
          let _s : string = 
            ocamlc ()
          in
            "false"))

(** Compute the default suffix for program (target OS dependent)
  *)
let suffix_program =
  var_define
    "suffix_program"
    (lazy
       (match os_type () with 
          | "Win32" -> ".exe" 
          | _ -> ""
       ))

(** Initialize some variables 
  *)
let init pkg = 
  rpkg := Some pkg

