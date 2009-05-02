
(** BaseInstall tools
    @author Sylvain Le Gall
  *)

open BaseInstall;;
open BaseUtils;;
open Format;;

module OASIS = OASISTypes;;

type module_name = string;;
type fun_name    = string;;
type field       = string;;

type ocaml_expr = 
  | REC of module_name * (field * ocaml_expr) list
  | LST of ocaml_expr list
  | STR of string
  | APP of fun_name * ocaml_expr list
;;

let rec pp_ocaml_expr fmt =
  function
    | REC (mod_nm, lst) ->
        pp_record_open fmt ();
        List.iter
          (fun (fld, e) ->
             pp_record_field fmt (mod_nm^"."^fld) pp_ocaml_expr e)
          lst;
        pp_record_close fmt ()
    | LST lst ->
        pp_ocaml_list pp_ocaml_expr fmt lst
    | STR str ->
        pp_ocaml_string fmt str
    | APP (fnm, args) ->
        pp_open_hvbox fmt 2;
        pp_print_string fmt fnm;
        List.iter
          (fun e ->
             pp_print_space fmt ();
             pp_ocaml_expr fmt e)
          args;
        pp_close_box fmt ()
;;

let library_code_of_oasis (nm, lib) =
  REC 
    ("BaseInstall",
     ["lib_name",      STR nm;
      "lib_buildable", LST [];
      "lib_modules",   LST (List.map 
                              (fun s -> STR s) 
                              lib.OASIS.lib_modules);
      "lib_path",      STR lib.OASIS.lib_path])
;;

let executable_code_of_oasis (nm, exec) = 
  REC 
    ("BaseInstall",
     ["exec_name",      STR nm;
      "exec_buildable", LST [];
      "exec_path",      STR (Filename.dirname exec.OASIS.exec_main_is)])
;;

