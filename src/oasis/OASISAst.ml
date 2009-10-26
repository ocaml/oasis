
(** AST manipulation
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

(** Convert oasis AST into package 
  *)
let to_package fn ignore_unknown srcdir ast = 

  let default_ctxt =
    {
      oasisfn     = fn;
      srcdir      = Filename.dirname fn;
      cond        = None;
      valid_flags = [];
    }
  in

  (* Convert flags into ctxt *)
  let ctxt_of_flags flags =
    {default_ctxt with 
         valid_flags = List.map fst flags}
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
   * depends from condition (if expression is possible
   *)
  let rec stmt wrtr ctxt =
    function
      | SField (nm, str) -> 
          (
            try
              OASISSchema.set_field wrtr nm ctxt str
            with (UnknownField _) as exc ->
              if ignore_unknown then
                ()
              else
                raise exc
          )

      | SIfThenElse (e, stmt1, stmt2) -> 
          (* Check that we have a valid expression *)
          OASISExpr.check ctxt e;
          (* Explore if branch *)
          stmt 
            wrtr 
            (ctxt_add_expr ctxt e)
            stmt1;
          (* Explore then branch *)
          stmt 
            wrtr 
            (ctxt_add_expr ctxt (ENot e))
            stmt2

      | SBlock blk ->
          List.iter (stmt wrtr ctxt) blk
  in

  (* Explore statement and register data into a newly created
   * Schema.writer.
   *)
  let schema_stmt gen nm schm flags stmt' = 
    let wrtr =
      OASISSchema.writer schm
    in
    let ctxt =
      ctxt_of_flags flags
    in
      stmt wrtr ctxt stmt';
      OASISSchema.check wrtr;
      gen nm wrtr
  in

  (* Recurse into top-level statement. At this level there is 
   * no conditional expression but there is Flag, Library and
   * Executable structure defined.
   *) 
  let rec top_stmt root_wrtr ((libs, execs, flags) as acc) =
    function
      | TSFlag (nm, stmt) -> 
          let flag =
            schema_stmt 
              OASISFlag.generator 
              nm
              OASISFlag.schema 
              flags
              stmt
          in
            libs, execs, (nm, flag) :: flags

      | TSLibrary (nm, stmt) -> 
          let lib = 
            schema_stmt 
              OASISLibrary.generator
              nm
              OASISLibrary.schema 
              flags 
              stmt
          in
            ((nm, lib) :: libs), execs, flags

      | TSExecutable (nm, stmt) -> 
          let exec =
            schema_stmt
              OASISExecutable.generator
              nm
              OASISExecutable.schema
              flags
              stmt
          in
            libs, ((nm, exec) :: execs), flags

      | TSStmt stmt' -> 
          stmt 
            root_wrtr 
            (ctxt_of_flags flags) 
            stmt';
          acc

      | TSBlock blk -> 
          List.fold_left 
            (top_stmt root_wrtr) 
            acc 
            blk
  in

  (* Start with package schema/writer *)
  let wrtr =
    OASISSchema.writer OASISPackage.schema 
  in
  let libs, execs, flags =
    top_stmt wrtr ([], [], []) ast
  in
  let pkg = 
    OASISPackage.generator wrtr libs execs flags
  in

  (* Fix build depends to reflect internal dependencies *)
  let pkg = 
    let map_internal_libraries what =
      List.map
        (function
           | (FindlibPackage (lnm, None)) as bd ->
               if List.mem_assoc lnm pkg.libraries then
                 InternalLibrary lnm
               else
                 bd
           | (FindlibPackage (lnm, Some _)) as bd ->
               if List.mem_assoc lnm pkg.libraries then
                 failwith 
                   (Printf.sprintf
                      "Cannot use versioned build depends \
                       on internal library %s in %s"
                      lnm
                      what)
               else
                 bd
           | (InternalLibrary _) as bd ->
               bd)
    in
      {pkg with 
           libraries = 
             List.map 
               (fun (nm, lib) ->
                  nm,
                  {lib with 
                       lib_build_depends = 
                         map_internal_libraries
                           ("library "^nm)
                           lib.lib_build_depends})
               pkg.libraries;
           executables =
             List.map 
               (fun (nm, exec) ->
                  nm,
                  {exec with 
                       exec_build_depends =
                         map_internal_libraries
                           ("executable "^nm)
                           exec.exec_build_depends})
               pkg.executables}
  in
    (* TODO: check recursion and re-order library/tools using ocamlgraph *)
    pkg
;;

