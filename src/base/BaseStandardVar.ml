
(** Most standard variables for OCaml 
    @author Sylvain Le Gall
  *)

open BaseCheck;;
open BaseEnv;;

(** {2 Paths} *)

(**/**)
let p name hlp dflt = 
  var_define
    ~short_desc:hlp 
    ~cli:CLIAuto 
    ~arg_help:"dir" 
    name 
    (lazy dflt) 

let (/) = Filename.concat

(**/**)

let prefix = 
  p "prefix"
    "install architecture-independent files dir"
    (match Sys.os_type with
       | "Win32" ->
           "%PROGRAMFILES%\\$pkg_name"
       | _ ->
           "/usr/local")
      
let exec_prefix = 
  p "exec_prefix"
    "Install architecture-dependent files in dir"
    "$prefix"

let bindir =
  p "bindir"
    "User executables"
    ("$exec_prefix"/"bin")

let sbindir =
  p "sbindir"
    "System admin executables"
    ("$exec_prefix"/"sbin")

let libexecdir =
  p "libexecdir"
    "Program executables"
    ("$exec_prefix"/"libexec")

let sysconfdir =
  p "sysconfdir"
    "Read-only single-machine data"
    ("$prefix"/"etc")

let sharedstatedir =
  p "sharedstatedir"
    "Modifiable architecture-independent data"
    ("$prefix"/"com")

let localstatedir =
  p "localstatedir"
    "Modifiable single-machine data"
    ("$prefix"/"var")

let libdir =
  p "libdir"
    "Object code libraries"
    ("$exec_prefix"/"lib")

let datarootdir =
  p "datarootdir"
    "Read-only arch.-independent data root"
    ("$prefix"/"share")

let datadir =
  p "datadir"
    "Read-only architecture-independent data"
    "$datarootdir"

let infodir =
  p "infodir"
    "Info documentation"
    ("$datarootdir"/"info")

let localedir =
  p "localedir"
    "Locale-dependent data"
    ("$datarootdir"/"locale")

let mandir =
  p "mandir"
    "Man documentation"
    ("$datarootdir"/"man")

let docdir =
  p "docdir"
    "Documentation root"
    ("$datarootdir"/"doc"/"$pkg_name")

let htmldir =
  p "htmldir"
    "HTML documentation"
    "$docdir"

let dvidir =
  p "dvidir"
    "DVI documentation"
    "$docdir"

let pdfdir =
  p "pdfdir"
    "PDF documentation"
    "$docdir"

let psdir =
  p "psdir"
    "PS documentation"
    "$docdir"

(** {2 Programs} *)

let ocamlfind  = BaseCheck.ocamlfind
let ocamlc     = BaseOCamlcConfig.ocamlc
let ocamlopt   = prog_opt "ocamlopt"
let ocamlbuild = prog "ocamlbuild"


(** {2 OCaml config variable} *) 

let c = BaseOCamlcConfig.var_define 

let ocaml_version            = c "version"
let standard_library_default = c "standard_library_default"
let standard_library         = c "standard_library"
let standard_runtime         = c "standard_runtime"
let ccomp_type               = c "ccomp_type"
let bytecomp_c_compiler      = c "bytecomp_c_compiler"
let native_c_compiler        = c "native_c_compiler"
let architecture             = c "architecture"
let model                    = c "model"
let system                   = c "system"
let ext_obj                  = c "ext_obj"
let ext_asm                  = c "ext_asm"
let ext_lib                  = c "ext_lib"
let ext_dll                  = c "ext_dll"
let os_type                  = c "os_type"
let default_executable_name  = c "default_executable_name"
let systhread_supported      = c "systhread_supported"

(** {2 ...} *)

(** Check what is the best target for platform (opt/byte)
  *)
let ocamlbest =
  var_define
    "ocamlbest"
    (lazy 
       (try
          let _s : string = 
            ocamlopt ()
          in
            "native"
        with Not_found ->
          let _s : string = 
            ocamlc ()
          in
           "byte"))

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

(** {2 Variables from OASIS package} 
  *)

(**/**)
let rpkg = 
  ref None

let pkg_get () =
  match !rpkg with 
    | Some pkg -> pkg
    | None -> 
        failwith 
          "OASIS Package is not set"
(**/**)

let pkg_name = 
  var_define
    ~short_desc:"Package name"
    "pkg_name"
    (lazy (fst (pkg_get ())))

let pkg_version =
  var_define
    ~short_desc:"Package version"
    "pkg_version"
    (lazy (snd (pkg_get ())))

(** Initialize some variables 
  *)
let init pkg = 
  rpkg := Some pkg

