
(** META generator for AutoBuild
    @author Sylvain Le Gall
  *)

open CommonGettext;;
open OASISTypes;;
open OASISValueParser;;
open BasePlugin;;
open BaseFileGenerate;;

let plugin_id = 
  "META"
;;

let description =
  OASIS.new_field
    OASISLibrary.schema
    plugin_id
    "Description"
    ~default:None
    (opt string_not_empty)
    (s_ "META package description")
;;

let enable = 
  OASIS.new_field
    OASISLibrary.schema
    plugin_id 
    "Enable"
    ~default:true
    boolean
    (s_ "Enable META generation")
;;

type predicate = string 
;;

type meta_t =
    {
      version:     string;
      description: string option;
      requires:    string list;
      archives:    (predicate list * filename list) list;
    }
;;

let main pkg =
  let version lib = 
    pkg.OASISTypes.version
  in

  let description lib =
    match description lib.lib_schema_data with 
      | Some txt ->
          Some txt
      | None ->
          Some pkg.synopsis
  in

  let requires lib =
    List.map
      (function
         | FindlibPackage (nm, _) 
         | InternalLibrary nm -> nm)
      pkg.build_depends 
  in

  let archives lib nm = 
    (["byte"], [nm^".cma"])
    ::
    (match lib.lib_compiled_object with
       | Best | Native ->
          [["native"], [nm^".cmxa"]]
      | Byte ->
          []
    )
  in

  let metas =
    List.fold_left
      (fun acc (nm, lib) ->
         if enable lib.lib_schema_data then
           (
             Filename.concat lib.lib_path "META",
             {
               version     = version lib;
               description = description lib;
               requires    = requires lib;
               archives    = archives lib nm;
             }
           )
           ::
           acc
         else
           acc)
      []
      pkg.libraries
  in
  let meta_content meta = 
    let meta_field nm vl acc =
      (Printf.sprintf "%s = %S" nm vl) :: acc
    in
      List.rev 
        (List.fold_left
           (fun acc f -> f acc)
           []
           (
             meta_field "version" meta.version ::
             meta_field "requires" (String.concat " " meta.requires) ::

             (match meta.description with 
                | Some txt -> meta_field "description" txt 
                | None -> (fun acc -> acc)) ::

             (List.map 
                (fun (preds, fns) -> 
                   meta_field 
                     ("archive("^(String.concat ", " preds)^")")
                     (String.concat " " fns))
                meta.archives)
           ))
  in


    List.iter 
      (fun (fn, meta) ->
         file_generate fn comment_meta (Split([], meta_content meta, [])))
      metas
;; 

plugin_register plugin_id (Extra main);;
