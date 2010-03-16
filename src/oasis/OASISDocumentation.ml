(** Test schema and generator
    @author Sylvain Le Gall
  *)

(* END EXPORT *)

open OASISTypes
open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext

let schema, generator =
  let schm =
   schema "Test"
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "test") schm
  in
  let build_tools = 
    OASISBuildSection.build_tools_field schm
  in
  let typ =
    new_field schm "Type"
      ~default:(OASISPlugin.builtin "none") 
      OASISPlugin.Test.value
      (fun () ->
         s_ "Plugin to use to build documentation.")
  in
  let install_dir =
    new_field schm "InstallDir"
      ~default:"$docdir"
      (expand file)
      (fun () ->
         s_ "Default target directory to install data and documentation.")
  in
  let build, install, data_files = 
   OASISBuildSection.build_install_data_fields schm
  in
    schm,
    (fun nm data ->
       Doc
         (cmn_section_gen nm data,
          {
            doc_type        = typ data;
            doc_build       = build data;
            doc_install     = install data;
            doc_install_dir = install_dir data;
            doc_data_files  = data_files data;
            doc_build_tools = build_tools data;
          }))
