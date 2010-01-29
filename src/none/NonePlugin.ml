
(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

let not_implemented str _ _ =
  failwith ("No implementation for "^str)

let section_not_implemented str pkg _ _ extra_args =
  not_implemented str pkg extra_args

(* END EXPORT *)

open BasePlugin

let plugin_id = "None"

let std_no_generate str pkg =
  {
    moduls       = [NoneData.nonesys_ml];
    setup        = func_with_arg 
                     not_implemented "NonePlugin.not_implemented"
                     str ODN.of_string;
    clean        = None;
    distclean    = None;
    other_action = ignore;
    files_generated  = [];
  },
  pkg

let section_no_generate str pkg nm section =
  let gen, pkg =
    std_no_generate 
      (str^" of section "^nm)
      pkg
  in
    gen,
    pkg,
    section

let () = 
  List.iter
    (plugin_register plugin_id)
    [
      Configure (std_no_generate "configure");
      Build     (std_no_generate "build"); 
      Doc       (section_no_generate "doc"); 
      Test      (section_no_generate "test"); 
      Install   (std_no_generate "install", 
                 std_no_generate "uninstall");
    ]
