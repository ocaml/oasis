
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* Return the name of the real name of executable, with full 
   unix path
 *)
let unix_exec_is (cs, bs, exec) is_native ext_dll = 
  let dir = 
    OASISUnixPath.concat
      bs.bs_path
      (OASISUnixPath.dirname exec.exec_main_is)
  in
  let is_native_exec = 
    match bs.bs_compiled_object with
      | Native -> true
      | Best -> is_native ()
      | Byte -> false
  in

    OASISUnixPath.concat
      dir
      cs.cs_name,

    if not is_native_exec && 
       not exec.exec_custom && 
       bs.bs_c_sources <> [] then
      Some (dir^"/dll"^cs.cs_name^(ext_dll ()))
    else
      None


(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext
open PropList.Field

let schema, generator =
  let schm =
    schema "Executable" 
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "executable") schm
  in
  let build_section_gen =
    OASISBuildSection.section_fields (s_ "executable") Byte schm
  in
  let main_is =
    new_field schm "MainIs" 
      (let base_value =
         regexp
           (Str.regexp ".*\\.ml$")
           (fun () -> s_ ".ml file")
       in
         {
           parse  = (fun str -> file.parse (base_value.parse str));
           update = update_fail;
           print  = (fun fn -> file.print (base_value.print fn));
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
    schm,
    (fun nm data -> 
       Executable
         (cmn_section_gen nm data,
          build_section_gen nm data,
          {
            exec_main_is = main_is data;
            exec_custom  = custom data;
          }))
