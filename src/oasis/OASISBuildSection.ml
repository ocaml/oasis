
(** Build section functions
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open CommonGettext

let build_depends_field schm = 
  new_field schm "BuildDepends" 
    ~default:[]
    (let base_value = 
       comma_separated 
         (with_optional_parentheses
            string_not_empty
            version_comparator)
     in
       {
         parse = 
           (fun str ->
              List.map 
                (fun (pkg, ver_constr_opt) -> 
                   FindlibPackage (pkg, ver_constr_opt))
                (base_value.parse str));
         print =
           (fun lst ->
              base_value.print
                (List.map 
                   (function 
                      | FindlibPackage (nm, ver) -> (nm, ver)
                      | InternalLibrary nm -> (nm, None))
                   lst));
       })
    (fun () -> s_ "Dependencies on findlib packages.")

let build_tools_field schm =
  new_field schm "BuildTools"
    ~default:[]
    (let base = 
       comma_separated file
     in
       {
         parse = 
           (fun str ->
              List.map 
                (fun s -> ExternalTool s) 
                (base.parse str));
         print =
           (fun lst ->
              base.print
                (List.map
                   (function
                      | InternalExecutable nm
                      | ExternalTool nm -> nm)
                lst))
       })
    (fun () -> s_ "Executables required to compile.")

let section_fields nm comp_dflt schm = 
  let path =
    new_field schm "Path" 
      directory
      (fun () ->
         Printf.sprintf
           (f_ "Directory containing the %s")
           nm)
  in
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf 
           (f_ "Set if the %s should be built. Use with flag.")
           nm)
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf
           (f_ "Set if the %s should be distributed.")
           nm)
  in
  let build_depends =
    build_depends_field schm
  in
  let build_tools =
    build_tools_field schm
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      (choices
         (fun () -> s_ "compiled object")
         ["byte", Byte; "native", Native; "best", Best])
      (fun () ->
         Printf.sprintf 
           (f_ "Define the compilation type of %s: byte, native or best")
           nm)
  in
  let c_sources = 
    new_field schm "CSources"
      ~default:[]
      files
      (fun () -> s_ "C source files.")
  in
  let data_files =
    new_field schm "DataFiles"
      ~default:[]
      (comma_separated
         (with_optional_parentheses
            (* TODO: these two strings are in fact "expendable strings" i.e. that
             * can contain $xxx, we need to check their correctness 
             *)
            string_not_empty
            string_not_empty))
      (fun () -> 
         s_ "Comma separated list of files to be installed for run-time use by \
             the package. Install by default in '$datadir/$pkg_name', you can \
             override using 'fn ($datadir/other_location)'. You can use \
             wildcard '*' but only for filename and followed by a single dot \
             extension: 'dir/*.html' is valid but 'dir/*' and 'dir/*.tar.gz' are \
             not valid.")
  in
    (fun nm data ->
       {
         bs_build           = build data;
         bs_install         = install data;
         bs_path            = path data;
         bs_compiled_object = compiled_object data;
         bs_build_depends   = build_depends data;
         bs_build_tools     = build_tools data;
         bs_c_sources       = c_sources data;
         bs_data_files      = data_files data;
       })
