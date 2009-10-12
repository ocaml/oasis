
(** Most standard variables for OCaml 
    @author Sylvain Le Gall
  *)

open BaseCheck;;
open BaseEnvironment;;

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
      
let eprefix = 
  p "eprefix"
    "Install architecture-dependent files in dir"
    "$prefix"

let bindir =
  p "bindir"
    "User executables"
    ("$eprefix"/"bin")

let sbindir =
  p "sbindir"
    "System admin executables"
    ("$eprefix"/"sbin")

let libexecdir =
  p "libexecdir"
    "Program executables"
    ("$eprefix"/"libexec")

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
    ("$eprefix"/"lib")

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

let c = BaseOCamlcConfig.var_cache 

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
let ocamlbest env =
  var_define
    "ocamlbest"
    (lazy 
       (try
          var_ignore (ocamlopt env);
          "native"
        with Not_found ->
          (var_ignore (ocamlc env);
           "byte")))
    env

(** Compute the default suffix for program (target OS dependent)
  *)
let suffix_program env =
  var_define
    "suffix_program"
    (lazy
       (match os_type env with 
          | "Win32" -> ".exe" 
          | _ -> ""
       ))
    env

(** Check against a minimal version.
  *)
let ocaml_version_constraint version_cmp env = 
  version 
    "ocaml" 
    version_cmp 
    (fun () -> ocaml_version env)
    env

(** All variables 
  *)
let all = 
  [
    prefix;
    eprefix;
    bindir;
    sbindir;
    libexecdir;
    sysconfdir;
    sharedstatedir;
    localstatedir;
    libdir;
    datarootdir;
    datadir;
    infodir;
    localedir;
    mandir;
    docdir;
    htmldir;
    dvidir;
    pdfdir;
    psdir;
    ocamlfind;
    ocamlc;
    ocamlopt;
    ocamlbuild;
    ocamlbest;
    ocaml_version;
    standard_library_default;
    standard_library;
    standard_runtime;
    ccomp_type;
    bytecomp_c_compiler;
    native_c_compiler;
    architecture;
    model;
    system;
    ext_obj;
    ext_asm;
    ext_lib;
    ext_dll;
    os_type;
    default_executable_name;
    systhread_supported;
    suffix_program;
  ]
