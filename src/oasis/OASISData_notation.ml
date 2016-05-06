(******************************************************************************)
(* ocaml-data-notation: Store data using OCaml notation                       *)
(*                                                                            *)
(* Copyright (C) 2009-2011, OCamlCore SARL                                    *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
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
  (** Integer *)
  | INT of int
  (** Float *)
  | FLT of float
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

(** {2 Basic conversion}*)

type 'a conv = 'a -> t

let unit () = UNT

let bool b = BOO b

let string s = STR s

let int i = INT i

let float f = FLT f

let option f =
  function
    | Some v -> VRT("Some", [f v])
    | None   -> VRT("None", [])

let list f lst = LST (List.map f lst)

let tuple2 f1 f2 (v1, v2) = TPL [f1 v1; f2 v2]

let tuple3 f1 f2 f3 (v1, v2, v3) = TPL [f1 v1; f2 v2; f3 v3]

let tuple4 f1 f2 f3 f4 (v1, v2, v3, v4) =
  TPL [f1 v1; f2 v2; f3 v3; f4 v4]

let tuple5 f1 f2 f3 f4 f5 (v1, v2, v3, v4, v5) =
  TPL [f1 v1; f2 v2; f3 v3; f4 v4; f5 v5]

let vrt0 s = VRT (s,[])
let vrt1 f1 s x1 = VRT (s, [f1 x1])
let vrt2 f1 f2 s x1 x2 = VRT (s, [f1 x1; f2 x2])
let vrt3 f1 f2 f3 s x1 x2 x3 = VRT (s, [f1 x1; f2 x2; f3 x3])

(** {2 Functions} *)

type 'a func =
  { func_call: 'a;
    func_name: string;
    func_arg:  t option;
  }

let func f f_nm =
  { func_call = f;
    func_name = f_nm;
    func_arg  = None;
  }

let func_with_arg f f_nm arg odn_of_arg =
  { func_call = f arg;
    func_name = f_nm;
    func_arg  = Some (odn_of_arg arg);
  }

let serialize_func t =
  match t.func_arg with
    | Some arg -> APP (t.func_name, [], [arg])
    | None -> VAR t.func_name

let func_call t = t.func_call

(** {2 Formatting} *)

type 'a printer = Format.formatter -> 'a -> unit

open Format

let pp ?(opened_modules=[]) fmt t =
  let opened_modules =
    (* Use opened modules starting with the bigger *)
    List.sort
      (fun mod1 mod2 -> String.length mod2 - String.length mod1)
      opened_modules
  in
  let pp_list pp_elem lst_sep fmt =
    function
      | [] -> ()
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
        let str_len = String.length str in
        let matching_opened_mod =
          List.find
            (fun opened_mod ->
               let opened_mod_len = String.length opened_mod in
               if opened_mod_len + 1 <= str_len
               then
                 (opened_mod = String.sub str 0 opened_mod_len)
                 &&
                 str.[opened_mod_len] = '.'
               else false)
            opened_modules
        in
        let chop_prefix_len = (String.length matching_opened_mod) + 1 in
        String.sub str chop_prefix_len (str_len - chop_prefix_len)
      with Not_found ->
        str
    in pp_print_string fmt (chop_opened_module id)
  in
  let rec aux fmt =
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
                aux e
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
          (pp_list aux ";@ ") lst
      | STR str ->
        fprintf fmt "%S" str
      | VRT (nm, []) ->
        pp_print_id fmt nm
      | VRT (nm, lst) ->
        fprintf fmt
          "@[<hv 2>%a@ %a@]"
          pp_print_id nm
          aux (TPL lst)
      | BOO b ->
        pp_print_bool fmt b
      | TPL [] ->
        pp_print_string fmt "()"
      | TPL [(FLT _) as v]
      | TPL [(INT _) as v]
      | TPL [(STR _) as v]
      | TPL [(REC _) as v]
      | TPL [(LST _) as v]
      | TPL [(BOO _) as v]
      | TPL [UNT as v]
      | TPL [(VAR _) as v] ->
        aux fmt v
      | TPL lst ->
        fprintf fmt "@[<hv 2>(%a)@]" (pp_list aux ",@ ") lst
      | UNT ->
        pp_print_string fmt "()"
      | FLT f ->
        pp_print_float fmt f
      | INT i ->
        pp_print_int fmt i
      | APP (fnm, named_args, args) ->
        fprintf fmt
          "@[<hv 2>%a%a%a@]"
          pp_print_id fnm
          (pp_list (fun fmt (nm, e) -> fprintf fmt "@ ~%s:%a" nm aux e) "")
          named_args
          (pp_list (fun fmt e -> fprintf fmt "@ %a" aux e) "")
          args
      | VAR nm ->
        pp_print_id fmt nm
      | PVR (nm, None) ->
        pp_print_id fmt ("`"^nm)
      | PVR (nm, Some tpl) ->
        fprintf fmt
          "@[<hv 2>`%a@ %a@]"
          pp_print_id nm
          aux tpl
  in
  aux fmt t

let to_string ?opened_modules odn =
  FormatExt.to_string (pp ?opened_modules) odn


