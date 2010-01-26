
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open CommonGettext
open PropList.Field

let schema, generator =
  let schm =
    schema "executable" 
  in
  let main_is =
    new_field schm "MainIs" 
      (let base_value =
         regexp
           (Str.regexp ".*\\.ml$")
           (fun () -> s_ ".ml file")
       in
         {
           parse = (fun str -> file.parse (base_value.parse str));
           print = (fun fn -> file.print (base_value.print fn));
         })
      (fun () -> 
         s_ "OCaml file (.ml) containing main procedure for the executable.")
  in
  let custom =
    new_field schm "Custom"
      ~default:false
      boolean
      (fun () ->
         s_ "Create custom bytecode executable.")
  in
  let build, install, compiled_object = 
    std_field (s_ "executable") Byte schm
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
    (fun nm data -> 
       {
         exec_build           = build data;
         exec_install         = install data;
         exec_main_is         = main_is data;
         exec_compiled_object = compiled_object data;
         exec_build_depends   = build_depends data;
         exec_build_tools     = build_tools data;
         exec_c_sources       = c_sources data;
         exec_custom          = custom data;
         exec_data_files      = data_files data;
         exec_is              = FilePath.concat 
                                  (FilePath.dirname (main_is data))
                                  nm;
         exec_schema_data     = data;
       })
