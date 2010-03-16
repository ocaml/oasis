
(** AST manipulation
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISAstTypes
open OASISUtils
open OASISGettext

(** Convert OASIS stream into package 
  *)
let to_package conf st = 

  let ast = 
    OASISRecDescParser.parse_stream 
      conf 
      st
  in

  let default_ctxt =
    {
      cond        = None;
      append      = false;
      valid_flags = [];
    }
  in

  (* Convert flags into ctxt *)
  let ctxt_of_sections scts =
    {default_ctxt with 
         valid_flags = 
           (List.fold_left 
              (fun acc ->
                function 
                  | Flag (cs, _) -> cs.cs_name :: acc
                  | _ -> acc)
              default_ctxt.valid_flags
              scts)}
  in

  (* Merge an expression with a condition in a ctxt *)
  let ctxt_add_expr ctxt e =
    match ctxt with 
      | {cond = None} ->
          {ctxt with cond = Some e}
      | {cond = Some e'} ->
          {ctxt with cond = Some (EAnd (e', e))}
  in

  (* Explore statement, at this level it is possible that value
   * depends from condition (if expression is possible)
   *)
  let rec stmt schm data ctxt =
    function
      | SField (nm, op) -> 
          begin
            try
              match op with
                | FSet s ->
                    begin
                      PropList.Schema.set 
                        schm 
                        data 
                        nm 
                        ~context:{ctxt with append = false}
                        s
                    end
                | FAdd s ->
                    begin
                      PropList.Schema.set 
                        schm 
                        data 
                        nm 
                        ~context:{ctxt with append = true}
                        s
                    end
                | FEval e ->
                    begin
                      PropList.Schema.set
                        schm
                        data
                        nm
                        ~context:{ctxt with append = false}
                        (string_of_bool false);
                      PropList.Schema.set 
                        schm 
                        data
                        nm 
                        ~context:{(ctxt_add_expr ctxt e) with append = false}
                        (string_of_bool true)
                    end
            with (PropList.Unknown_field _) as exc ->
              if OASISPlugin.test_field_name nm && conf.ignore_unknown then
                ()
              else
                raise exc
          end

      | SIfThenElse (e, stmt1, stmt2) -> 
          begin
            (* Check that we have a valid expression *)
            OASISExpr.check ctxt e;
            (* Explore if branch *)
            stmt 
              schm
              data
              (ctxt_add_expr ctxt e)
              stmt1;
            (* Explore then branch *)
            stmt 
              schm
              data 
              (ctxt_add_expr ctxt (ENot e))
              stmt2
          end

      | SBlock blk ->
          List.iter (stmt schm data ctxt) blk
  in

  (* Explore statement and register data into a newly created
   * Schema.writer.
   *)
  let schema_stmt gen nm schm scts stmt' = 
    let data =
      PropList.Data.create ()
    in
    let ctxt =
      ctxt_of_sections scts
    in
      stmt schm data ctxt stmt';
      OASISCheck.check_schema 
        (schm.PropList.Schema.name ^" "^nm)
        schm 
        data;
      (gen nm data) :: scts
  in

  (* Recurse into top-level statement. At this level there is 
   * no conditional expression but there is Flag, Library and
   * Executable structure defined.
   *) 
  let rec top_stmt pkg_data acc =
    function
      | TSLibrary (nm, stmt) -> 
          schema_stmt 
            OASISLibrary.generator
            nm
            OASISLibrary.schema 
            acc
            stmt

      | TSExecutable (nm, stmt) -> 
          schema_stmt
            OASISExecutable.generator
            nm
            OASISExecutable.schema
            acc
            stmt

      | TSFlag (nm, stmt) -> 
          schema_stmt 
            OASISFlag.generator 
            nm
            OASISFlag.schema 
            acc
            stmt

      | TSSourceRepository (nm, stmt) ->
          schema_stmt
            OASISSourceRepository.generator
            nm
            OASISSourceRepository.schema
            acc
            stmt

      | TSTest (nm, stmt) ->
          schema_stmt
            OASISTest.generator
            nm
            OASISTest.schema
            acc
            stmt

      | TSStmt stmt' -> 
          stmt 
            OASISPackage.schema
            pkg_data 
            (ctxt_of_sections acc) 
            stmt';
          acc

      | TSBlock blk -> 
          List.fold_left 
            (top_stmt pkg_data) 
            acc 
            blk
  in

  (* Start with package schema/writer *)
  let data =
    PropList.Data.create ()
  in
  let sections =
    top_stmt 
      data
      []
      ast
  in
  let pkg = 
    OASISCheck.check_schema 
      "package"
      OASISPackage.schema 
      data;
    OASISPackage.generator 
      data
      sections
  in

  (* Fix build depends to reflect internal dependencies *)
  let pkg = 
    (* Map of findlib name to internal libraries *)
    let internal_of_findlib =
      let mp = 
        OASISLibrary.findlib_name_map pkg
      in
        MapString.fold
          (fun nm _ acc -> 
             let fndlb_nm_full =
               OASISLibrary.findlib_of_name 
                 ~recurse:true 
                 mp 
                 nm
             in
               MapString.add fndlb_nm_full nm acc)
          mp
          MapString.empty
    in

    let map_internal_libraries what =
      List.map
        (function
           | (FindlibPackage (lnm, ver_opt)) as bd ->
               begin
                 try 
                   let lnm =
                     MapString.find lnm internal_of_findlib
                   in
                     if ver_opt <> None then
                       failwith 
                         (Printf.sprintf
                            "Cannot use versioned build depends \
                             on internal library %s in %s"
                            lnm
                            what);

                     InternalLibrary lnm

                 with Not_found ->
                   bd
               end
           | (InternalLibrary _) as bd ->
               bd)
    in

    let internal_tools = 
      List.fold_left
        (fun st ->
           function
             | Executable (cs, _, _) ->
                 SetString.add cs.cs_name st
             | _ ->
                 st)
        SetString.empty
        pkg.sections
    in

    let map_internal_tools what = 
      List.map 
        (function
           | ExternalTool lnm as bt ->
               begin
                 if SetString.mem lnm internal_tools then
                   InternalExecutable lnm
                 else
                   bt
               end
           | InternalExecutable _ as bt ->
               bt)
    in

    let map_internal what bs = 
      {bs with 
           bs_build_depends = map_internal_libraries what bs.bs_build_depends;
           bs_build_tools = map_internal_tools what bs.bs_build_tools}
    in

      {pkg with 
           sections =
             List.map 
               (function
                  | Library (cs, bs, lib) ->
                      Library 
                        (cs, 
                         map_internal 
                           (Printf.sprintf (f_ "library %s") cs.cs_name)
                           bs,
                         lib)
                  | Executable (cs, bs, exec) ->
                      Executable
                        (cs,
                         map_internal
                           (Printf.sprintf (f_ "executable %s") cs.cs_name)
                           bs,
                         exec)
                  | Test (cs, tst) ->
                      Test 
                        (cs,
                         {tst with 
                              test_build_tools = 
                                map_internal_tools 
                                  (Printf.sprintf "test %s" cs.cs_name)
                                  tst.test_build_tools})
                  | Flag _ | SrcRepo _ as sct ->
                      sct)
               pkg.sections}
  in

  (* Check recursion and re-order library/tools so that build order is
     respected 
   *)
  let pkg = 
    {pkg with sections = OASISBuildSection.build_order pkg}
  in

    pkg

