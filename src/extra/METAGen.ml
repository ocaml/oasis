
(** META generator
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISTypes
open OASISValues
open OASISLibrary
open BaseFileGenerate
open Format

module PU = OASISPlugin.Extra.Make
              (struct
                 let name = "META"
                 let version = OASISConf.version
               end)
open PU

let new_field nm = 
  new_field OASISLibrary.schema nm

let enable = 
  new_field
    "Enable"
    ~default:true
    boolean
    (fun () ->
       s_ "Enable META generation")

let description =
  new_field
    "Description"
    ~default:None
    (opt string_not_empty)
    (fun () ->
       s_ "META package description")

type meta_type =
  | METALibrary
  | METASyntax

let typ =
  new_field
    "Type"
    ~default:METALibrary
    (choices
       (fun () ->
          s_ "META type")
       [
         "library", METALibrary;
         "syntax",  METASyntax;
       ])
    (fun () ->
       s_ "Type of META package, set default predicates for archive")

let requires =
  new_field
    "Requires"
    ~default:None
    (opt (comma_separated string))
    (fun () ->
       s_ "Requires field for META package")

let pp_print_meta pkg findlib_name_map fmt grp =

  let pp_print_field fmt (var, preds, vl) = 
    fprintf fmt
      "@,@[<hv1>%s(@[%a@])@ =@ %S@]" 
      var 
      (FormatExt.pp_print_list pp_print_string ",@,") preds
      vl
  in
  let pp_print_sfield fmt (var, vl) = 
    fprintf fmt "@,@[<hv 1>%s@ =@ %S@]" var vl
  in

  let root_cs, root_bs, root_lib =
    root_of_group grp
  in

  let default_synopsis = 
    match description root_cs.cs_data with
      | Some txt -> txt
      | None -> pkg.synopsis
  in

  let rec pp_print_library fmt (lib_cs, lib_bs, lib, children) =
    let lib_name =
      lib_cs.cs_name
    in
      pp_print_sfield fmt ("version", (OASISVersion.string_of_version pkg.version));
      begin
        let txt =
          match description lib_cs.cs_data with
            | Some txt -> txt
            | None -> default_synopsis 
        in
          pp_print_sfield fmt ("description", txt)
      end;
      begin 
        let requires = 
          match requires lib_cs.cs_data with 
            | Some lst ->
                lst
            | None ->
                List.map 
                  (function
                     | InternalLibrary nm ->
                         OASISLibrary.findlib_of_name 
                           ~recurse:true
                           findlib_name_map 
                           nm
                     | FindlibPackage (nm, _) ->
                         nm)
                  lib_bs.bs_build_depends
        in
         if requires <> [] then
          pp_print_sfield fmt ("requires", String.concat " " requires)
      end; 
      begin
        match typ lib_cs.cs_data with 
          | METALibrary ->
              pp_print_field fmt ("archive", ["byte"], lib_name^".cma");
              begin
                match lib_bs.bs_compiled_object with
                  | Best | Native ->
                      pp_print_field fmt ("archive", ["native"], lib_name^".cmxa");
                  | Byte ->
                      ()
              end

          | METASyntax ->
              pp_print_field fmt ("archive", ["syntax"; "preprocessor"], lib_name^".cma");
              pp_print_field fmt ("archive", ["syntax"; "toploop"], lib_name^".cma")
      end;
      FormatExt.pp_print_list pp_print_group "@," fmt children

  and pp_print_group fmt = 
    function 
      | Container (fndlb_nm, children) ->
          fprintf fmt 
            "@,@[<hv1>package %S (%a%a@]@,)"
            fndlb_nm
            
            pp_print_sfield 
            ("description", "Virtual container")
            
            (FormatExt.pp_print_list pp_print_group "") children

      | Package (fndlb_nm, lib_cs, lib_bs, lib, children) ->
          if enable lib_cs.cs_data then
            fprintf fmt "@,@[<hv1>package %S (%a@]@,)"
              fndlb_nm
              pp_print_library (lib_cs, lib_bs, lib, children)
  in

    assert(enable root_cs.cs_data);
    pp_open_vbox fmt 0;
    fprintf fmt "# OASIS_START";
    begin
      match grp with 
        | Container (_, children) ->
            FormatExt.pp_print_list pp_print_group "" fmt children
        | Package (_, lib_cs, lib_bs, lib, children) ->
            pp_print_library fmt (lib_cs, lib_bs, lib, children)
    end;
    fprintf fmt "@,# OASIS_STOP@,";
    pp_close_box fmt ();
    pp_print_flush fmt ()

let main pkg =
  let findlib_name_map = 
    findlib_name_map pkg
  in
    List.iter 
      (fun grp ->
         let root_cs, root_bs, root_lib = 
           root_of_group grp
         in
           (* TODO: check that enable values are consistent *)
           if enable root_cs.cs_data then
             begin
               let meta_fn =
                 Filename.concat root_bs.bs_path "META"
               in
               let buff =
                 Buffer.create 13
               in
                 pp_print_meta 
                   pkg 
                   findlib_name_map 
                   (Format.formatter_of_buffer buff) 
                   grp;
                 file_generate 
                   meta_fn 
                   comment_meta 
                   (NeedSplit 
                      (ExtString.String.nsplit
                         (Buffer.contents buff)
                         "\n"))
             end)
      (group_libs pkg)

let () = 
  register main
