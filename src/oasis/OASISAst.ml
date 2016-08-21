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


(** AST manipulation
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISUtils
open OASISGettext
open OASISAst_types
open OASISExpr

(** Convert OASIS stream into package
*)
let to_package ~ctxt ast =

  (* Merge an expression with a condition in a ctxt *)
  let ctxt_add_expr ctxt e =
    match ctxt with
    | {cond = None; _} -> {ctxt with cond = Some e}
    | {cond = Some e'; _} -> {ctxt with cond = Some (EAnd (e', e))}
  in

  (* Explore statement, at this level it is possible that value
   * depends from condition (if expression is possible)
  *)
  let rec stmt schm data ctxt =
    function
    | SField (nm, op) ->
      begin
        try
          let set = PropList.Schema.set schm data nm in
          let ctxt = {ctxt with append = false} in
          match op with
          | FSet s ->
            set ~context:ctxt s
          | FAdd s ->
            set ~context:{ctxt with append =true}  s
          | FEval e ->
            set ~context:ctxt (string_of_bool false);
            set ~context:(ctxt_add_expr ctxt e) (string_of_bool true)
        with (PropList.Unknown_field _) as exc ->
          if OASISPlugin.test_field_name nm &&
             ctxt.ctxt.OASISContext.ignore_plugins then
            ()
          else if ctxt.ctxt.OASISContext.ignore_unknown_fields then
            ()
          else
            raise exc
      end

    | SIfThenElse (e, stmt1, stmt2) ->
      (* Check that we have a valid expression *)
      OASISExpr.check ctxt.valid_flags e;
      (* Explore if branch *)
      stmt schm data (ctxt_add_expr ctxt e) stmt1;
      (* Explore then branch *)
      stmt schm data (ctxt_add_expr ctxt (ENot e)) stmt2

    | SBlock blk -> List.iter (stmt schm data ctxt) blk
  in

  (* Explore statement and register data into a newly created
   * Schema.writer.
  *)
  let schema_stmt gen schm (ctxt, scts) nm stmt' =
    let data = PropList.Data.create () in
    let schm = schm.OASISSchema_intern.schm in
    let where = (PropList.Schema.name schm)^" "^nm in
    stmt schm data ctxt stmt';
    ctxt,
    (schm, where, data, fun features_data -> gen features_data nm data) :: scts
  in

  (* Recurse into top-level statement. At this level there is no conditional
   * expression but there is Flag, Library and Executable structure defined.
  *)
  let rec top_stmt pkg_data (ctxt, _ as acc) =
    function
      | TSSection (sct_knd, nm, stmt) -> section sct_knd nm acc stmt
      | TSBlock blk -> List.fold_left (top_stmt pkg_data) acc blk
      | TSStmt stmt' ->
        stmt OASISPackage.schema.OASISSchema_intern.schm pkg_data ctxt stmt';
        acc
  and section sct_knd nm (ctxt, scts) stmt =
    let gen =
      match sct_knd with
      | `Library ->
        schema_stmt OASISLibrary_intern.generator OASISLibrary.schema
      | `Object ->
        schema_stmt OASISObject_intern.generator OASISObject.schema
      | `Executable ->
        schema_stmt OASISExecutable_intern.generator OASISExecutable.schema
      | `Flag ->
        schema_stmt OASISFlag_intern.generator OASISFlag.schema
      | `Test ->
        schema_stmt OASISTest_intern.generator OASISTest.schema
      | `Doc ->
        schema_stmt OASISDocument_intern.generator OASISDocument.schema
      | `SrcRepo ->
        schema_stmt
          OASISSourceRepository_intern.generator
          OASISSourceRepository.schema
    in
    let acc =
      if sct_knd = `Flag then
        {ctxt with valid_flags = nm :: ctxt.valid_flags}, scts
      else
        ctxt, scts
    in
    gen acc nm stmt
  in

  (* Interpret AST and inject it into data. *)
  let data = PropList.Data.create () in
  let ctxt, sections =
    let default_ctxt =
      {
        cond        = None;
        append      = false;
        valid_flags = [];
        ctxt        = ctxt;
      }
    in
    top_stmt data (default_ctxt, []) ast
  in


  (* Extract features_data *)
  let features_data =
    let oasis_version =
      try
        OASISPackage_intern.oasis_version data
      with PropList.Not_set _ ->
        failwith (s_ "OASISFormat not defined at the beginning of the file, \
                      consider starting with 'OASISFormat: ...'")
    in
    OASISFeatures.Data.create oasis_version
      (OASISPackage_intern.alpha_features data)
      (OASISPackage_intern.beta_features data)
  in

  (* Check all schema. *)
  let () =
    let plugins =
      OASISCheck.check_schema ~ctxt:ctxt.ctxt
        "package"
        OASISPackage.schema.OASISSchema_intern.schm
        OASISPlugin.SetPlugin.empty
        features_data
        data
    in
    List.iter
      (fun (schm, where, data, _) ->
         let _plugins: OASISPlugin.SetPlugin.t =
           OASISCheck.check_schema ~ctxt:ctxt.ctxt
             where schm plugins features_data data
         in
         ())
      sections
  in

  let pkg =
    let sections =
      List.map (fun (_, _, _, gen) -> gen features_data) sections
    in
    OASISPackage_intern.generator data sections
  in

  (* Fix build depends to reflect internal dependencies *)
  let pkg =
    (* Map of findlib name to internal libraries *)
    let _, _, internal_of_findlib =
      OASISFindlib.findlib_mapping pkg
    in

    let map_internal_libraries sct =
      List.map
        (function
          | (FindlibPackage (lnm, ver_opt)) as bd ->
            let is_internal, lnm =
              try
                true, internal_of_findlib lnm
              with (OASISFindlib.FindlibPackageNotFound _) ->
                false, lnm
            in
            if is_internal then begin
              if ver_opt <> None then
                failwithf
                  (f_ "Cannot use versioned build depends \
                       on internal library %s in %s")
                  lnm (OASISSection.string_of_section sct);
              InternalLibrary lnm
            end else begin
              bd
            end
          | (InternalLibrary _) as bd ->
            bd)
    in

    let internal_tools =
      List.fold_left
        (fun st ->
           function
           | Executable (cs, _, _) -> SetString.add cs.cs_name st
           | _ -> st)
        SetString.empty
        pkg.sections
    in

    let map_internal_tools _ =
      List.map
        (function
          | ExternalTool lnm as bt ->
            if SetString.mem lnm internal_tools then
              InternalExecutable lnm
            else
              bt
          | InternalExecutable _ as bt ->
            bt)
    in

    let map_internal sct bs =
      {bs with
         bs_build_depends =
           map_internal_libraries sct bs.bs_build_depends;
         bs_build_tools =
           map_internal_tools sct bs.bs_build_tools}
    in

    {pkg with
       sections =
         List.map
           (function
             | Library (cs, bs, lib) as sct ->
               Library (cs, map_internal sct bs, lib)
             | Object (cs, bs, obj) as sct ->
               Object (cs, map_internal sct bs, obj)
             | Executable (cs, bs, exec) as sct ->
               Executable
                 (cs, map_internal sct bs, exec)
             | Test (cs, tst) as sct ->
               Test (cs,
                     {tst with
                      test_tools = map_internal_tools sct tst.test_tools})
             | Doc (cs, doc) as sct ->
               Doc (cs,
                    {doc with
                     doc_build_tools =
                       map_internal_tools sct doc.doc_build_tools})
             | Flag _ | SrcRepo _ as sct ->
               sct)
           pkg.sections}
  in

  (* Check recursion and re-order library/tools so that build order is
     respected
  *)
  let pkg = {pkg with sections = OASISBuildSection.build_order pkg} in
  OASISCheck.check_package ~ctxt:ctxt.ctxt pkg;
  pkg

