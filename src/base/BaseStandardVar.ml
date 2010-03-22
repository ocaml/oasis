
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
    (lazy dflt) 

let (/) = Filename.concat

(**/**)

let prefix = 
  p "prefix"
    (s_ "Install architecture-independent files dir")
    (* TODO: we should use os_type () here rather than Sys.os_type *)
    (match Sys.os_type with
       | "Win32" ->
           "%PROGRAMFILES%\\$pkg_name"
       | _ ->
           "/usr/local")
      
let exec_prefix = 
  p "exec_prefix"
    (s_ "Install architecture-dependent files in dir")
    "$prefix"

let bindir =
  p "bindir"
    (s_ "User executables")
    ("$exec_prefix"/"bin")

let sbindir =
  p "sbindir"
    (s_ "System admin executables")
    ("$exec_prefix"/"sbin")

let libexecdir =
  p "libexecdir"
    (s_ "Program executables")
    ("$exec_prefix"/"libexec")

let sysconfdir =
  p "sysconfdir"
    (s_ "Read-only single-machine data")
    ("$prefix"/"etc")

let sharedstatedir =
  p "sharedstatedir"
    (s_ "Modifiable architecture-independent data")
    ("$prefix"/"com")

let localstatedir =
  p "localstatedir"
    (s_ "Modifiable single-machine data")
    ("$prefix"/"var")

let libdir =
  p "libdir"
    "Object code libraries"
    ("$exec_prefix"/"lib")

let datarootdir =
  p "datarootdir"
    (s_ "Read-only arch-independent data root")
    ("$prefix"/"share")

let datadir =
  p "datadir"
    (s_ "Read-only architecture-independent data")
    "$datarootdir"

let infodir =
  p "infodir"
    (s_ "Info documentation")
    ("$datarootdir"/"info")

let localedir =
  p "localedir"
    (s_ "Locale-dependent data")
    ("$datarootdir"/"locale")

let mandir =
  p "mandir"
    (s_ "Man documentation")
    ("$datarootdir"/"man")

let docdir =
  p "docdir"
    (s_ "Documentation root")
    ("$datarootdir"/"doc"/"$pkg_name")

let htmldir =
  p "htmldir"
    (s_ "HTML documentation")
    "$docdir"

let dvidir =
  p "dvidir"
    (s_ "DVI documentation")
    "$docdir"

let pdfdir =
  p "pdfdir"
    (s_ "PDF documentation")
    "$docdir"

let psdir =
  p "psdir"
    (s_ "PS documentation")
    "$docdir"

let destdir =
  var_define
    ~short_desc:(s_ "Prepend a path when installing package")
    ~cli:CLIAuto 
    ~arg_help:"dir" 
    "destdir"
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
    ~short_desc:(s_ "Package name")
    "pkg_name"
    (lazy (fst (pkg_get ())))

let pkg_version =
  var_define
    ~short_desc:(s_ "Package version")
    "pkg_version"
    (lazy 
       (OASISVersion.string_of_version 
          (snd (pkg_get ()))))

(** Initialize some variables 
  *)
let init pkg = 
  rpkg := Some pkg

