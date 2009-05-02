
(** OCaml code generation 
    @author Sylvain Le Gall
  *)

open BaseUtils;;
open Format;;

type module_name = string;;
type fun_name    = string;;
type field       = string;;
type var_name    = string;;

type ocaml_expr = 
  | REC of module_name * (field * ocaml_expr) list
  | LST of ocaml_expr list
  | STR of string
  | APP of fun_name * ocaml_expr list
  | VRT of string * ocaml_expr list
  | BOO of bool
  | TPL of ocaml_expr list
  | VAR of string
  | FUN of var_name list * ocaml_stmts
  | UNT
and ocaml_stmts =
  ocaml_expr list
;;

let pp_list pp_elem lst_sep fmt =
  function
    | [] ->
        ()
    | hd :: tl ->
        pp_elem fmt hd;
        List.iter
          (fun e ->
             fprintf fmt lst_sep;
             pp_elem fmt e)
          tl
;;

let rec pp_ocaml_expr fmt =
  function
    | REC (mod_nm, lst) ->
        fprintf fmt "@[{@[<hv1>@,";
        List.iter
          (fun (fld, e) ->
             fprintf fmt "@[<hv2>%s.%s =@ (%a)@];@ " 
               mod_nm fld pp_ocaml_expr e)
          lst;
        fprintf fmt "@]@,}@]"
    | LST lst ->
        fprintf fmt "@[[@[<hv1>@,%a@]@,]@]"
          (pp_list pp_ocaml_expr ";@ ") lst
    | STR str ->
        fprintf fmt "%S" str
    | APP (fnm, args) ->
        pp_open_hvbox fmt 2;
        pp_print_string fmt fnm;
        List.iter
          (fun e ->
             pp_print_space fmt ();
             pp_ocaml_expr fmt e)
          args;
        pp_close_box fmt ()
    | VRT (nm, lst) ->
        pp_open_hvbox fmt 2;
        pp_print_string fmt nm;
        pp_print_space fmt ();
        pp_ocaml_expr fmt (TPL lst);
        pp_close_box fmt ()
    | BOO b ->
        pp_print_bool fmt b
    | TPL [] ->
        invalid_arg "BaseGenCode.TPL []"
    | TPL [v] ->
        pp_ocaml_expr fmt v
    | TPL (hd :: tl) ->
        pp_open_hvbox fmt 2;
        pp_print_char fmt '(';
        pp_ocaml_expr fmt hd;
        List.iter
          (fun v ->
             pp_print_char fmt ',';
             pp_print_space fmt ();
             pp_ocaml_expr fmt v)
          tl;
        pp_print_char fmt ')';
        pp_close_box fmt ()
    | VAR nm ->
        pp_print_string fmt nm
    | FUN (var_lst, stmts) ->
        pp_open_hvbox fmt 2;

        pp_open_hvbox fmt 2;
        pp_print_string fmt "fun";
        pp_print_space fmt ();
        pp_list (fun fmt nm -> pp_ocaml_expr fmt (VAR nm)) "@ " fmt var_lst;
        pp_print_space fmt ();
        pp_print_string fmt "->";
        pp_close_box fmt ();

        pp_print_space fmt ();
        pp_ocaml_stmts fmt stmts;
        pp_close_box fmt ()
    | UNT ->
        pp_print_string fmt "()"

and pp_ocaml_stmts fmt =
  function
    | [] ->
        pp_ocaml_expr fmt UNT
    | lst ->
        pp_open_hvbox fmt 0;
        pp_list pp_ocaml_expr ";@ " fmt lst;
        pp_close_box fmt ()
;;

