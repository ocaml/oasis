
(** Load and query _oasis file
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand
open BaseMessage
open Genlex
open OASISTypes
open OASISSection
open OASISUtils

let queries =
  ref []

let separator = 
  ref "\n"

let lexer = 
  make_lexer 
    ["ListSections"; "ListFields"; "("; ")"; "."]

let query pkg str = 

  let assoc_sections = 
    [
      "Library",    
      (KLibrary, 
       OASISLibrary.schema.OASISSchema.schm);

      "Executable", 
      (KExecutable, 
       OASISExecutable.schema.OASISSchema.schm);

      "Flag",
      (KFlag, 
       OASISFlag.schema.OASISSchema.schm);

      "SrcRepo", 
      (KSrcRepo,
       OASISSourceRepository.schema.OASISSchema.schm);

      "Test",
      (KTest, 
       OASISTest.schema.OASISSchema.schm);

      "Doc",
      (KDoc, 
       OASISDocument.schema.OASISSchema.schm);
    ]
  in

  let mk_section sct = 
    let (knd, nm) =
      OASISSection.section_id sct
    in
    let start, (_, schm) = 
      List.find 
        (fun (str, (knd', _)) -> knd = knd')
        assoc_sections
    in
    let fmt = 
      if OASISUtils.is_varname nm then  
        Printf.sprintf "%s(%s)" 
      else 
        Printf.sprintf "%s(%S)"
    in
      fmt start nm, schm
  in

  let parse_id_or_string =
    parser
      | [< 'Ident str >] ->
          str
      | [< 'String str >] ->
          str
  in

  let parse_fld_or_section start_nm = 
    parser 
      | [< 'Kwd "("; nm = parse_id_or_string; 'Kwd ")"; 
           'Kwd "."; 
           fld = parse_id_or_string >] ->          
          begin
            let kind, schm = 
              try 
                List.assoc 
                  (String.lowercase start_nm)
                  (List.map 
                     (fun (nm, e) -> String.lowercase nm, e) 
                     assoc_sections)
              with Not_found ->
                failwithf2
                  (f_ "Don't know section kind '%s' in query '%s'")
                  start_nm str
            in
            let sct  =
              OASISSection.section_find
                (kind, nm) 
                pkg.sections
            in
            let data = 
              (OASISSection.section_common sct).cs_data
            in
              schm, data, fld
          end

      | [< >] ->
          begin
            (* We have a single field *)
            OASISPackage.schema.OASISSchema.schm,
            pkg.schema_data,
            start_nm
          end
  in

  let parse = 
    parser 
      | [< start_nm = parse_id_or_string; 
           (schm, data, fld) = parse_fld_or_section start_nm >] ->
          begin
            PropList.Schema.get schm data fld
          end

      | [< 'Kwd "ListSections" >] ->
          begin
            String.concat 
              !separator
              (List.map 
                 (fun sct -> fst (mk_section sct))
                 pkg.sections)
          end

      | [< 'Kwd "ListFields" >] ->
          begin
            let fold_schm prefix schm data acc =
              PropList.Schema.fold
                (fun acc nm _ _ ->
                   try 
                     let _v : string = 
                       PropList.Schema.get schm data nm
                     in
                       (prefix^nm) :: acc
                   with _ ->
                     acc)
                acc
                schm
            in
            let lst = 
              List.fold_left
                (fun acc sct ->
                   let prefix, schm = mk_section sct in
                   let data = (section_common sct).cs_data in
                     fold_schm (prefix^".") schm data acc)

                (* Start with the package fields *)
                (fold_schm "" OASISPackage.schema.OASISSchema.schm pkg.schema_data [])

                (* Continue with section fields *)
                pkg.sections
            in

              String.concat 
                !separator 
                (List.rev lst)
          end

  in
    parse (lexer (Stream.of_string str))


let main () = 
  let pkg = 
    OASISParse.from_file
      ~ctxt:!BaseContext.default
      ~ignore_plugins:!ArgCommon.ignore_plugins
      !ArgCommon.oasis_fn
  in
  let answers = 
    List.rev_map (query pkg) !queries
  in
    print_endline (String.concat !separator answers)


let scmd =
  {(SubCommand.make 
      ~std_usage:false
      "query"
      (s_ "Query an _oasis file")
      CLIData.query_mkd
      main) 
     with
         scmd_usage = 
           s_ "[options*] query*";
         scmd_anon = 
           (fun e -> queries := e :: !queries);
         scmd_specs =
           ([
             "-separator",
             Arg.Set_string separator,
             s_ "str String to add between answers."
           ]
           @ ArgCommon.oasis_fn_specs @ ArgCommon.ignore_plugins_specs)}

let () = 
  SubCommand.register scmd
