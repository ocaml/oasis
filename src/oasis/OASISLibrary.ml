
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator =
  let schm =
    schema "Library"
  in
  let path =
    new_field schm "Path" 
      directory_exists
      (s_ "Directory containing the library")
  in
  let modules =
    new_field schm "Modules" 
      ~default:[]
      modules
      (s_ "List of modules to compile.") 
  in
  let build, install, compiled_object = 
    OASISUtils.std_field (s_ "library") Best schm
  in
  let build_depends, build_tools =
    OASISUtils.depends_field schm
  in
  let c_sources = 
    OASISUtils.c_field schm
  in
  let data_files = 
    OASISUtils.data_field schm
  in
    schm,
    (fun (_: string) wrtr ->
       {
         lib_build           = build wrtr;
         lib_install         = install wrtr;
         lib_path            = path wrtr;
         lib_modules         = modules wrtr;
         lib_compiled_object = compiled_object wrtr;
         lib_build_depends   = build_depends wrtr;
         lib_build_tools     = build_tools wrtr;
         lib_c_sources       = c_sources wrtr;
         lib_data_files      = data_files wrtr;
         lib_schema_data     = wrtr;
       })
;;
