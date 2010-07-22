open OASISTypes

let unix_exec_is (cs, bs, exec) is_native ext_dll suffix_program = 
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
      (cs.cs_name^(suffix_program ())),

    if not is_native_exec && 
       not exec.exec_custom && 
       bs.bs_c_sources <> [] then
      Some (dir^"/dll"^cs.cs_name^(ext_dll ()))
    else
      None

(* END EXPORT *)

let schema = OASISExecutable_intern.schema
