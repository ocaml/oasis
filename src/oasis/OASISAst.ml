
(** AST manipulation
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

(** Convert oasis AST into package 
  *)
let to_package fn srcdir valid_tests ast = 

  let lowercase_eq str1 str2 =
    (String.lowercase str1) = (String.lowercase str2)
  in

  let default_ctxt =
    {
      oasisfn     = fn;
      srcdir      = Filename.dirname fn;
      cond        = None;
      valid_flags = [];
      valid_tests = valid_tests;
    }
  in

  (* Convert flags into ctxt *)
  let ctxt_of_flags flags =
    {default_ctxt with 
         valid_flags = List.map fst flags}
  in

  (* Check that expression only use valid tests/flags *)
  let rec expr_check ctxt =
    function
      | EBool _ -> 
          ()
      | ENot e -> 
          expr_check ctxt e 
      | EAnd (e1, e2) | EOr (e1, e2) -> 
          expr_check ctxt e1; 
          expr_check ctxt e2
      | EFlag nm ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_flags) then
              failwith 
                (Printf.sprintf 
                   "Unknown flag '%s'"
                   nm)
          )
      | ETest (nm, vl) ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_tests) then
              failwith 
                (Printf.sprintf 
                   "Unknown test '%s'"
                   nm)
          )
  in

  (* Merge an expresion with a condition in a ctxt *)
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
          OASISSchema.set_field wrtr nm ctxt str

      | SIfThenElse (e, stmt1, stmt2) -> 
          (* Check that we have a valid expression *)
          expr_check ctxt e;
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
  let schema_stmt gen schm flags stmt' = 
    let wrtr =
      OASISSchema.writer schm
    in
    let ctxt =
      ctxt_of_flags flags
    in
      stmt wrtr ctxt stmt';
      OASISSchema.check wrtr;
      gen wrtr
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
              OASISFlag.schema 
              flags
              stmt
          in
            libs, execs, (nm, flag) :: flags

      | TSLibrary (nm, stmt) -> 
          let lib = 
            schema_stmt 
              OASISLibrary.generator
              OASISLibrary.schema 
              flags 
              stmt
          in
            ((nm, lib) :: libs), execs, flags

      | TSExecutable (nm, stmt) -> 
          let exec =
            schema_stmt
              OASISExecutable.generator
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
    OASISPackage.generator wrtr libs execs flags
;;

