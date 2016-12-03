
open OASISPlugin
open OASISPluginVersionFileGettext
open OASISValues
open OASISSchema
open OASISTypes

type t =
  {
    filename: string;
  }

let version_short = OASISVersion.chop OASISPluginVersionFileConf.version
let plugin = `Extra, "VersionFile", Some version_short
let self_id, all_id = Extra.create plugin

let feature_name =
  OASISFeatures.create "filename" ~plugin
    (OASISFeatures.since_version "0.1")
    (fun () ->
       s_ "Allow to choose which file to generate.")

let pivot_data = data_new_property plugin

let generator =
  let new_field nm =
    new_field OASISPackage.schema all_id nm
  in
  let filename =
    new_field
      "Filename"
      ~default:"version.ml"
      ~feature:feature_name
      string
      (fun () ->
         s_ "File to generate where the version will be stored")
      pivot_data (fun _ t -> t.filename)
  in

  fun data ->
    {
      filename = filename data;
    }

let main ctxt pkg =
  let t = generator pkg.schema_data in
  let content =
    Printf.sprintf
      "let version = %S"
      (OASISVersion.string_of_version pkg.version)
  in
  let open OASISFileTemplate in
  OASISPlugin.add_file
    (template_of_mlfile
       t.filename
       [] (* header *)
       [content] (* body *)
       [] (* footer *))
    ctxt

let init () =
  Extra.register_act self_id main;
  register_help
    plugin
    {(help_default OASISPluginVersionFileData.readme_template_mkd) with
       help_order = 40};
  register_generator_package all_id pivot_data generator

let () = init ()
