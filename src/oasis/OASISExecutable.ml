
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator =
  let schm =
    schema "executable" 
  in
  let main_is =
    new_field schm "MainIs" 
      (fun ctxt vl ->
         str_regexp
           (Str.regexp ".*\\.ml$")
           (s_ ".ml file")
           ctxt
           (file_exists ctxt vl))
      (s_ "OCaml file (.ml) containing main procedure for the executable.")
  in
  let custom =
    new_field schm "Custom"
      ~default:false
      boolean
      (s_ "Create custom bytecode executable.")
  in
  let build, install, compiled_object = 
    OASISUtils.std_field (s_ "executable") Byte schm
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
    (fun nm wrtr -> 
       {
         exec_build           = build wrtr;
         exec_install         = install wrtr;
         exec_main_is         = main_is wrtr;
         exec_compiled_object = compiled_object wrtr;
         exec_build_depends   = build_depends wrtr;
         exec_build_tools     = build_tools wrtr;
         exec_c_sources       = c_sources wrtr;
         exec_custom          = custom wrtr;
         exec_data_files      = data_files wrtr;
         exec_is              = FilePath.concat 
                                  (FilePath.dirname (main_is wrtr))
                                  nm;
         exec_schema_data     = wrtr;
       })
;;

