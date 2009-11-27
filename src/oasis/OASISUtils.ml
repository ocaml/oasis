
(** Various utilities for OASIS.
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let std_field nm comp_dflt schm = 
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (Printf.sprintf 
         (f_ "Set if the %s should be built. Use with flag.")
         nm)
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (Printf.sprintf
         (f_ "Set if the %s should be distributed.")
         nm)
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      compiled_object
      (Printf.sprintf 
         (f_ "Define the compilation type of %s: byte, native or best")
         nm)
  in
    build, 
    install,
    compiled_object
;;

let depends_field schm = 
  let build_depends = 
    new_field schm "BuildDepends" 
      ~default:[]
      build_depends
      (s_ "Dependencies on findlib packages and internal libraries.")
  in
  let build_tools =
    new_field schm "BuildTools"
      ~default:[]
      comma_separated
      (s_ "Executables require to compile.")
  in
    build_depends, build_tools
;;

let c_field schm = 
  new_field schm "CSources"
    ~default:[]
    files
    (s_ "C source files.")
;;

let data_field schm =
  new_field schm "DataFiles"
    ~default:[]
    data_files
    (s_ "Comma separated list of files to be installed for run-time use by \
         the package. Install by default in '$datadir/$pkg_name', you can \
         override using 'fn ($datadir/other_location)'. You can use \
         wildcard '*' but only for filename and followed by a single dot \
         extension: 'dir/*.html' is valid but 'dir/*' and 'dir/*.tar.gz' are \
         not valid.")
;;
