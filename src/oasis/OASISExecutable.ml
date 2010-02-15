
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* Return the directory that will contain the executable *)
let exec_main_path (cs, bs, exec) = 
  let dir = 
    Filename.dirname exec.exec_main_is
  in
    if dir = Filename.current_dir_name then
      bs.bs_path
    else
      Filename.concat bs.bs_path dir

(* Return the name of the real name of executable, with full 
   path
 *)
let exec_is ((cs, _, _) as exec_data) = 
  Filename.concat (exec_main_path exec_data) cs.cs_name

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
  let build_section_gen =
    OASISBuildSection.section_fields (s_ "executable") Byte schm
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "executable") schm
  in
    schm,
    (fun nm data -> 
       Executable
         (cmn_section_gen nm data,
          build_section_gen nm data,
          {
            exec_main_is = main_is data;
            exec_custom  = custom data;
          }))
