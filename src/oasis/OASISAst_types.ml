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


(** AST types
    @author Sylvain Le Gall
*)


open OASISTypes

(* TODO: get rid of that if possible. *)
(** Context for parsing and checking AST *)
type ctxt =
  {
    (** Current condition for conditional fields. *)
    cond: OASISExpr.t option;

    (** Valid flags *)
    valid_flags: name list;

    (** Combine values rather than setting it, when
        setting field values
    *)
    append: bool;

    (** Global context *)
    ctxt: OASISContext.t;
  }

(** Abstract Syntax Tree *)
type field_op =
  | FSet of string
  | FAdd of string
  | FEval of OASISExpr.t


type stmt =
  | SField of name * field_op
  | SIfThenElse of OASISExpr.t * stmt * stmt
  | SBlock of stmt list


type top_stmt =
  | TSSection of section_kind * name * stmt
  | TSStmt of stmt
  | TSBlock of top_stmt list


let norm =
  let rec norm_top_stmt =
    function
    | TSSection(sct_knd, nm, stmt) -> TSSection(sct_knd, nm, norm_stmt stmt)
    | TSStmt stmt -> TSStmt(norm_stmt stmt)
    | TSBlock [tstmt] -> norm_top_stmt tstmt
    | TSBlock lst -> TSBlock(norm_flatten_tsblock lst)
  and norm_flatten_tsblock =
    function
    | (TSBlock l1) :: l2 -> norm_flatten_tsblock (l1 @ l2)
    | hd :: tl -> norm_top_stmt hd :: norm_flatten_tsblock tl
    | [] -> []
  and norm_stmt =
    function
    | SField(nm, fop) -> SField(nm, norm_field_op fop)
    | SIfThenElse(expr, stmt1, stmt2) ->
      SIfThenElse(OASISExpr.reduce expr, norm_stmt stmt1, norm_stmt stmt2)
    | SBlock [stmt] -> norm_stmt stmt
    | SBlock lst -> SBlock(norm_flatten_sblock lst)
  and norm_flatten_sblock =
    function
    | (SBlock l1) :: l2 -> norm_flatten_sblock (l1 @ l2)
    | hd :: tl -> norm_stmt hd :: norm_flatten_sblock tl
    | [] -> []
  and norm_field_op =
    function
    | FSet _ as e -> e
    | FAdd _ as e -> e
    | FEval expr -> FEval (OASISExpr.reduce expr)
  in
  norm_top_stmt
