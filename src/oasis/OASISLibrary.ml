
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValues;;
open OASISUtils;;
open CommonGettext;;
open PropList.Field;;

let schema, generator =
  let schm =
    schema "Library"
  in
  let path =
    new_field schm "Path" 
      directory
      (fun () ->
         s_ "Directory containing the library")
  in
  let modules =
    new_field schm "Modules" 
      ~default:[]
      ~quickstart_level:Beginner
      modules
      (fun () ->
         s_ "List of modules to compile.") 
  in
  let build, install, compiled_object = 
    std_field (s_ "library") Best schm
  in
  let build_depends, build_tools =
    depends_field schm
  in
  let c_sources = 
    c_field schm
  in
  let data_files = 
    data_field schm
  in
    schm,
    (fun (_: string) data ->
       {
         lib_build           = build data;
         lib_install         = install data;
         lib_path            = path data;
         lib_modules         = modules data;
         lib_compiled_object = compiled_object data;
         lib_build_depends   = build_depends data;
         lib_build_tools     = build_tools data;
         lib_c_sources       = c_sources data;
         lib_data_files      = data_files data;
         lib_schema_data     = data;
       })
;;
