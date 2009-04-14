
(** AST manipulation
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

(** Evaluate expression *)
let rec expr_eval ctxt =
  function 
    | ETrue  ->
        true
    | EFalse -> 
        false
    | ENot e -> 
        expr_eval ctxt e 
    | EAnd (e1, e2) ->
        (expr_eval ctxt e1) && (expr_eval ctxt e2)
    | EOr (e1, e2) -> 
        (expr_eval ctxt e1) || (expr_eval ctxt e2)
    | EFlag nm ->
        (
          (* TODO *)
          false
        )
    | ETest (nm, vl) ->
        (
          (* TODO *)
          false
        )
;;

(** Check oasis AST *)
let check valid_tests fn srcdir ast = 

  let lowercase_eq str1 str2 =
    (String.lowercase str1) = (String.lowercase str2)
  in

  (* Check AST *)
  let rec check_expr ctxt =
    function
      | ETrue | EFalse -> 
          ()
      | ENot e -> 
          check_expr ctxt e 
      | EAnd (e1, e2) | EOr (e1, e2) -> 
          check_expr ctxt e1; 
          check_expr ctxt e2
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

  let rec check_stmt wrtr ctxt =
    function
      | SField (nm, str) -> 
          OASISSchema.set_field wrtr nm ctxt str
      | SIfThenElse (e, blk1, blk2) ->
          check_expr ctxt e;
          check_stmt wrtr ctxt blk1;
          check_stmt wrtr ctxt blk2
      | SBlock blk ->
          List.iter (check_stmt wrtr ctxt) blk
  in

  let rec check_top_stmt root_wrtr ctxt =
    function
      | TSFlag (nm, blk) -> 
          let ctxt = 
            check_schema OASISFlag.schema ctxt blk
          in
            {ctxt with valid_flags = nm :: ctxt.valid_flags}
      | TSLibrary (_, blk) -> 
          check_schema OASISLibrary.schema ctxt blk
      | TSExecutable (_, blk) ->
          check_schema OASISExecutable.schema ctxt blk
      | TSStmt stmt ->
          check_stmt root_wrtr ctxt stmt; 
          ctxt
      | TSBlock lst ->
          List.fold_left 
            (check_top_stmt root_wrtr) 
            ctxt
            lst
  and check_schema schm ctxt blk =
    let wrtr =
      OASISSchema.writer schm
    in
      check_stmt wrtr ctxt blk;
      OASISSchema.check wrtr;
      ctxt
  in

    check_top_stmt 
      (OASISSchema.writer OASISPackage.schema)
      {
        oasisfn     = fn;
        srcdir      = Filename.dirname fn;
        valid_flags = [];
        valid_tests = valid_tests;
      }
      ast
;;

let oasis (ast, ctxt) =
  let rec oasis_stmt wrtr =
    function
      | SField (nm, str) -> 
          OASISSchema.set_field wrtr nm ctxt str

      | SIfThenElse (e, blk1, blk2) -> 
          let blk =
            if expr_eval ctxt e then
              blk1 
            else
              blk2
          in
            oasis_stmt wrtr blk

      | SBlock blk ->
          List.iter (oasis_stmt wrtr) blk
  in

  let rec oasis_top_stmt root_wrtr ((libs, execs, flags) as acc) =
    function
      | TSFlag (nm, blk) -> 
          let wrtr =
            OASISSchema.writer OASISFlag.schema
          in
          let flag =
            oasis_stmt wrtr blk;
            OASISFlag.generator wrtr
          in
            libs, execs, (nm, flag) :: flags

      | TSLibrary (nm, blk) -> 
          let wrtr =
            OASISSchema.writer OASISLibrary.schema
          in
          let lib = 
            oasis_stmt wrtr blk;
            OASISLibrary.generator wrtr
          in
            ((nm, lib) :: libs), execs, flags

      | TSExecutable (nm, blk) -> 
          let wrtr =
            OASISSchema.writer OASISExecutable.schema
          in
          let exec = 
            oasis_stmt wrtr blk;
            OASISExecutable.generator wrtr
          in
            libs, ((nm, exec) :: execs), flags

      | TSStmt stmt -> 
          oasis_stmt root_wrtr stmt;
          acc

      | TSBlock blk -> 
          List.fold_left (oasis_top_stmt root_wrtr) acc blk
  in
  let wrtr =
    OASISSchema.writer OASISPackage.schema 
  in
  let libs, execs, flags =
    oasis_top_stmt wrtr ([], [], []) ast
  in
    OASISPackage.generator wrtr libs execs flags
;;

