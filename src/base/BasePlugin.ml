
(** Manage plugin for AutoBuild
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open CommonGettext;;

(** Type for OCaml module embedded code
  *)
type modul = string;;

(** Describe action made by a target
  *)
type generator_action =
    { 
      (** OCaml module to be added to setup.ml *)
      moduls: modul list;

      (** Function to be added to BaseSetup.t *)
      setup_code: BaseGenCode.ocaml_expr;

      (** Function to be called when cleaning *)
      clean_code: BaseGenCode.ocaml_stmts;

      (** Function to be called when distcleaning *)
      distclean_code: BaseGenCode.ocaml_stmts;

      (** Write extra files *)
      other_action: unit -> unit; 

      (** Files generated *)
      files_generated: filename list;
    }
;;

(** Standard steps action
  *)
type std_act_t = 
    package -> generator_action * package
;;

(** Configure step action
  *)
type conf_act_t = 
    package -> generator_action
;;

(** Test step action
  *)
type test_act_t =
    test -> generator_action * test
;;

(** Kind of plugin 
  *)
type plugin_t =
  | Configure of conf_act_t
  | Build     of std_act_t 
  | Doc       of std_act_t
  | Test      of test_act_t
  | Install   of std_act_t * std_act_t (* Install and uninstall data *)
  | Extra     of (package -> unit)
;;

module MapPlugin = Map.Make (
struct 
  type t = string 

  let compare nm1 nm2 = 
    String.compare 
      (String.lowercase nm1)
      (String.lowercase nm2)
end)
;; 

let (configure_plugins,
     build_plugins,
     doc_plugins,
     test_plugins,
     install_plugins,
     uninstall_plugins,
     extra_plugins) =
  let p () = 
    ref MapPlugin.empty
  in
    p (),
    p (),
    p (),
    p (),
    p (),
    p (),
    p () 
;;

(** Register a new plugin *)
let plugin_register nm =
  let padd rmp e = 
    rmp := MapPlugin.add nm e !rmp
  in
    function
      | Configure e -> padd configure_plugins e
      | Build e     -> padd build_plugins     e
      | Doc e       -> padd doc_plugins       e
      | Test e      -> padd test_plugins      e
      | Extra e     -> padd extra_plugins     e
      | Install (i, u) -> 
          padd install_plugins i;
          padd uninstall_plugins u
;;

(**/**)
let plugin_get rmp err_fmt nm =
  try
    MapPlugin.find nm !rmp
  with Not_found ->
    (
      let availables =
        MapPlugin.fold (fun k _ acc -> k :: acc) !rmp []
      in
        failwith 
          (Printf.sprintf 
             err_fmt 
             nm 
             (String.concat ", " availables))
    )
;;

let plugin_ls rmp = 
  List.rev
    (MapPlugin.fold 
       (fun k _ lst -> k :: lst)
       !rmp
       [])
;;
(**/**)

(** Get configure plugin 
  *)
let plugin_configure = 
  plugin_get 
    configure_plugins 
    (f_ "Unkown configure plugin '%s' (available: %s)")
;;

(** Get a build plugin 
  *)
let plugin_build = 
  plugin_get 
    build_plugins
    (f_ "Unkown build plugin '%s' (available: %s)")
;;

(** Get a doc plugin 
  *)
let plugin_doc = 
  plugin_get 
    doc_plugins
    (f_ "Unkown doc plugin '%s' (available: %s)")
;;

(** Get a test plugin 
  *)
let plugin_test = 
  plugin_get 
    test_plugins
    (f_ "Unkown test plugin '%s' (available: %s)")
;;

(** Get a install plugin 
  *)
let plugin_install = 
  plugin_get 
    install_plugins
    (f_ "Unkown install plugin '%s' (available: %s)")
;;

(** Get an uninstall plugin
  *)
let plugin_uninstall = 
  plugin_get 
    uninstall_plugins 
    (f_ "Unknown uninstall plugin '%s' (available: %s)")
;;

(** Get a extra plugin 
  *)
let plugin_extra = 
  plugin_get 
    extra_plugins
    (f_ "Unkown plugin '%s' (available: %s)")
;;

(** Convert target_kind to string 
  *)
let string_of_plugin =
  function
    | Configure _ -> "configure"
    | Build _     -> "build"
    | Doc _       -> "doc"
    | Test _      -> "test"
    | Install _   -> "install"
    | Extra _     -> "extra"
;;


