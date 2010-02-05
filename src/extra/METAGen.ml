
(** META generator
    @author Sylvain Le Gall
  *)

open CommonGettext
open OASISTypes
open OASISValues
open OASISLibrary
open BasePlugin
open BaseFileGenerate
open Format

let plugin_id = "META"

let enable = 
  OASIS.new_field
    OASISLibrary.schema
    plugin_id 
    "Enable"
    ~default:true
    boolean
    (fun () ->
       s_ "Enable META generation")

let description =
  OASIS.new_field
    OASISLibrary.schema
    plugin_id
    "Description"
    ~default:None
    (opt string_not_empty)
    (fun () ->
       s_ "META package description")

type meta_type =
  | METALibrary
  | METASyntax

let typ =
  OASIS.new_field
    OASISLibrary.schema
    plugin_id
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
  OASIS.new_field
    OASISLibrary.schema
    plugin_id
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

  let _, root_lib =
    root_of_group grp
  in

  let default_synopsis = 
    match description root_lib.lib_schema_data with
      | Some txt -> txt
      | None -> pkg.synopsis
  in

  let rec pp_print_library fmt (lib_name, lib, children) =
    pp_print_sfield fmt ("version", (OASISVersion.string_of_version pkg.version));
    begin
      let txt =
        match description lib.lib_schema_data with
          | Some txt -> txt
          | None -> default_synopsis 
      in
        pp_print_sfield fmt ("description", txt)
    end;
    begin 
      let requires = 
        match requires lib.lib_schema_data with 
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
                lib.lib_build_depends
      in
       if requires <> [] then
        pp_print_sfield fmt ("requires", String.concat " " requires)
    end; 
    begin
      match typ lib.lib_schema_data with 
        | METALibrary ->
            pp_print_field fmt ("archive", ["byte"], lib_name^".cma");
            begin
              match lib.lib_compiled_object with
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

      | Package (fndlb_nm, nm, lib, children) ->
          if enable lib.lib_schema_data then
            fprintf fmt "@,@[<hv1>package %S (%a@]@,)"
              fndlb_nm
              pp_print_library (nm, lib, children)
  in

    assert(enable root_lib.lib_schema_data);
    pp_open_vbox fmt 0;
    fprintf fmt "# OASIS_START";
    begin
      match grp with 
        | Container (_, children) ->
            FormatExt.pp_print_list pp_print_group "" fmt children
        | Package (_, nm, lib, children) ->
            pp_print_library fmt (nm, lib, children)
    end;
    fprintf fmt "@,# OASIS_STOP@,";
    pp_close_box fmt ();
    pp_print_flush fmt ()

let main pkg =
  let findlib_name_map = 
    findlib_name_map pkg.libraries
  in
    List.iter 
      (fun grp ->
         let _, root_lib = 
           root_of_group grp
         in
           (* TODO: check that enable values are consistent *)
           if enable root_lib.lib_schema_data then
             begin
               let meta_fn =
                 Filename.concat root_lib.lib_path "META"
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
      (group_libs pkg.libraries)

let () = 
  plugin_register 
    plugin_id 
    (Extra main)
