(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
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
  | TSLibrary of name * stmt
  | TSObject of name * stmt
  | TSExecutable of name * stmt
  | TSFlag of name * stmt
  | TSSourceRepository of name * stmt
  | TSTest of name * stmt
  | TSDocument of name * stmt
  | TSStmt of stmt
  | TSBlock of top_stmt list

