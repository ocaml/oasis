(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


(** OCaml data notation.

    This module helps to translate OCaml data into a string following
    OCaml syntax.
  *)

(** {2 Types}
  *)

type module_name  = string
type field_name   = string
type variant_name = string
type var_name     = string

type t =
  (** Record *)
  | REC of module_name * (field_name * t) list
  (** List *)
  | LST of t list
  (** String *)
  | STR of string
  (** Variant type constructor *)
  | VRT of variant_name * t list
  (** Boolean *)
  | BOO of bool
  (** Tuple *)
  | TPL of t list
  (** Unit () *)
  | UNT
  (** Function application *)
  | APP of var_name * (var_name * t) list * t list
  (** Variable *)
  | VAR of var_name
  (** Polymorphic variant *)
  | PVR of variant_name * t option

(** {2 Basic conversion}
  *)

let of_unit () = UNT
let of_bool b = BOO b
let of_string s = STR s

let of_option f =
  function
    | Some v -> VRT("Some", [f v])
    | None   -> VRT("None", [])

let of_list f lst =
  LST (List.map f lst)


(** {2 Function conversion}
*)

(** Function that can be generated using
    func_call = APP(func, [], [func_arg]).
*)
type 'a func =
  {
    func_call: 'a;
    func_name: string;
    func_arg:  t option;
  }


(** Return the OCaml function corresponding to a [func].
*)
let func f f_nm =
  {
    func_call = f;
    func_name = f_nm;
    func_arg  = None;
  }


(** Create a func with an argument
*)
let func_with_arg f f_nm arg odn_of_arg =
  {
    func_call = f arg;
    func_name = f_nm;
    func_arg  = Some (odn_of_arg arg);
  }

let func_with_arg_ctxt f f_nm arg odn_of_arg =
  {
    func_call = (fun ~ctxt -> f ~ctxt arg);
    func_name = f_nm;
    func_arg  = Some (odn_of_arg arg);
  }

(** Return the [t] code corresponding to a [func].
*)
let odn_of_func t =
  match t.func_arg with
    | Some arg ->
      APP (t.func_name, [], [arg])
    | None ->
      VAR t.func_name


(** Return the OCaml function corresponding to a [func].
*)
let func_call t = t.func_call

(** {2 Formating}
  *)

open Format

let pp_odn ?(opened_modules=[]) fmt t =

  let opened_modules =
    (* Use opened modules starting with the bigger *)
    List.sort
      (fun mod1 mod2 -> String.length mod2 - String.length mod1)
      opened_modules
  in

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
  in

  let pp_print_id fmt id =
    let chop_opened_module str =
      try
        let str_len =
          String.length str
        in

        let matching_opened_mod =
          List.find
            (fun opened_mod ->
               let opened_mod_len =
                 String.length opened_mod
               in
                 if opened_mod_len + 1 <= str_len then
                   (opened_mod = String.sub str 0 opened_mod_len)
                   &&
                   str.[opened_mod_len] = '.'
                 else
                   false)
            opened_modules
        in

        let chop_prefix_len =
          (String.length matching_opened_mod) + 1
        in

          String.sub str chop_prefix_len (str_len - chop_prefix_len)

      with Not_found ->
        str
    in
      pp_print_string fmt (chop_opened_module id)
  in

  let rec pp_odn_aux fmt =
    function
      | REC (mod_nm, flds) ->
          begin
            let rec print_fields fmt first fields =
              let print_field fmt (fld, e) =
                fprintf fmt
                  "@[<hv 2>%a =@ %a@]"
                  (* We use the first field to add the module name at the
                   * beginning. *)
                  pp_print_id (if first then mod_nm^"."^fld else fld)
                  pp_odn_aux e
              in
              match fields with
                | [fld, e] ->
                    print_field fmt (fld, e)
                | (fld, e) :: tl ->
                    print_field fmt (fld, e);
                    fprintf fmt ";@ ";
                    print_fields fmt false tl
                | [] ->
                    ()
            in
              fprintf fmt "@[{@[<hv 2>@,";
              print_fields fmt true flds;
              fprintf fmt "@]@,}@]"
          end

      | LST lst ->
          fprintf fmt "@[[@[<hv 2>@,%a@]@,]@]"
            (pp_list pp_odn_aux ";@ ") lst
      | STR str ->
          fprintf fmt "%S" str
      | VRT (nm, []) ->
          pp_print_id fmt nm
      | VRT (nm, lst) ->
          fprintf fmt
            "@[<hv 2>%a@ %a@]"
            pp_print_id nm
            pp_odn_aux (TPL lst)
      | BOO b ->
          pp_print_bool fmt b
      | TPL [] ->
          pp_print_string fmt "()"
      | TPL [(STR _) as v]
      | TPL [(REC _) as v]
      | TPL [(LST _) as v]
      | TPL [(BOO _) as v]
      | TPL [UNT as v]
      | TPL [(VAR _) as v] ->
          pp_odn_aux fmt v
      | TPL lst ->
          fprintf fmt
            "@[<hv 2>(%a)@]"
            (pp_list pp_odn_aux ",@ ") lst
      | UNT ->
          pp_print_string fmt "()"
      | APP (fnm, named_args, args) ->
          fprintf fmt
            "@[<hv 2>%a%a%a@]"
            pp_print_id fnm

            (pp_list
               (fun fmt (nm, e) ->
                  fprintf fmt "@ ~%s:%a" nm pp_odn_aux e) "")
            named_args

            (pp_list
               (fun fmt e ->
                  fprintf fmt "@ %a" pp_odn_aux e) "")
            args
      | VAR nm ->
          pp_print_id fmt nm
      | PVR (nm, None) ->
          pp_print_id fmt ("`"^nm)
      | PVR (nm, Some tpl) ->
          fprintf fmt
            "@[<hv 2>`%a@ %a@]"
            pp_print_id nm
            pp_odn_aux tpl
  in

    pp_odn_aux fmt t
