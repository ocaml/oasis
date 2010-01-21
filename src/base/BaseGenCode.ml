
(** OCaml code generation and pretty printing
    @author Sylvain Le Gall
  *)

open BaseUtils;;
open Format;;
open FormatExt;;

type module_name = string;;
type fun_name    = string;;
type field       = string;;
type var_name    = string;;

type ocaml_expr = 
  (** Record *)
  | REC of module_name * (field * ocaml_expr) list
  (** List *)
  | LST of ocaml_expr list
  (** String *)
  | STR of string
  (** Function application *)
  | APP of fun_name * (var_name * ocaml_expr) list * ocaml_expr list 
  (** Variant type constructor *)
  | VRT of string * ocaml_expr list
  (** Boolean *)
  | BOO of bool
  (** Tuple *)
  | TPL of ocaml_expr list
  (** Variable *)
  | VAR of string
  (** Function definition *)
  | FUN of var_name list * ocaml_stmts
  (** Unit () *)
  | UNT
and ocaml_stmts =
  ocaml_expr list
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
          (pp_print_list pp_ocaml_expr ";@ ") lst
    | STR str ->
        fprintf fmt "%S" str
    | APP (fnm, named_args, args) ->
        pp_open_hvbox fmt 2;
        pp_print_string fmt fnm;
        List.iter
          (fun (nm, e) ->
             fprintf fmt "@ ~%s:%a" 
               nm
               pp_ocaml_expr e)
          named_args;
        List.iter
          (fun e ->
             pp_print_space fmt ();
             pp_ocaml_expr fmt e)
          args;
        pp_close_box fmt ()
    | VRT (nm, []) ->
        pp_print_string fmt nm
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
        pp_print_list (fun fmt nm -> pp_ocaml_expr fmt (VAR nm)) "@ " fmt var_lst;
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
        pp_print_list pp_ocaml_expr ";@ " fmt lst;
        pp_close_box fmt ()
;;

