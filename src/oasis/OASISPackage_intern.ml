(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


(** Package schema and generator
    @author Sylvain Le Gall
*)


open OASISTypes


(* END EXPORT *)


open OASISValues
open OASISUtils
open OASISSchema_intern
open OASISGettext
open OASISExpr


let mod_build_depends f pkg =
  {pkg with
     sections =
       List.map
         (function
           | Library (cs, bs, lib) ->
             Library (cs, f bs, lib)
           | Object (cs, bs, obj) ->
             Object (cs, f bs, obj)
           | Executable (cs, bs, exec) ->
             Executable (cs, f bs, exec)
           | Test _ | Flag _ | SrcRepo _ | Doc _ as sct ->
             sct)
         pkg.sections}


let add_build_depend build_depend pkg =
  mod_build_depends
    (fun bs ->
       {bs with bs_build_depends = build_depend :: bs.bs_build_depends})
    pkg


let add_build_tool ?(no_test=false) ?(condition=[EBool true, true])
    build_tool pkg =
  let pkg =
    mod_build_depends
      (fun bs ->
         {bs with bs_build_tools = build_tool :: bs.bs_build_tools})
      pkg
  in
  if no_test then
    pkg
  else
    {pkg with
       sections =
         List.map
           (function
             | Test (cs, test) ->
               Test (cs,
                 {test with
                    test_tools =
                      build_tool :: test.test_tools})
             | Doc (cs, doc) ->
               Doc (cs,
                 {doc with
                    doc_build_tools =
                      build_tool :: doc.doc_build_tools})
             | Library _ | Object _
             | Executable _ | Flag _ | SrcRepo _ as sct ->
               sct)
           pkg.sections}


let schema =
  schema "Package" (fun pkg -> pkg.plugin_data)


let oasis_version =
  let current_version =
    OASISConf.version_short
  in
  let extra_supported_versions =
    List.map
      OASISVersion.version_of_string
      ["0.3"; "0.2"; "0.1"]
  in
  new_field schema "OASISFormat"
    ~quickstart_level:(NoChoice current_version)
    {
      parse =
        (fun ~ctxt str ->
           let v =
             OASISVersion.value.parse ~ctxt str
           in
           if not
               (List.mem
                  v
                  (current_version :: extra_supported_versions)) then
             failwithf
               (f_ "OASIS format version '%s' is not supported.")
               str;
           v);
      update = update_fail;
      print = OASISVersion.value.print;
    }
    (fun () ->
       s_ "OASIS format version used to write file `_oasis`.")
    (fun pkg -> pkg.oasis_version)


let alpha_features, beta_features =
  let value_feature stage =
    {
      parse =
        (fun ~ctxt feature_name ->
           match OASISFeatures.get_stage feature_name with
             | OASISFeatures.InDev feature_stage ->
               if feature_stage <> stage then
                 failwithf (f_ "Feature %s is in stage %s and not %s.")
                   feature_name
                   (OASISFeatures.string_of_stage feature_stage)
                   (OASISFeatures.string_of_stage stage)
               else
                 feature_name
             | OASISFeatures.SinceVersion min_version ->
               failwithf
                 (f_ "Features %s has been published in OASISVersion %s.")
                 feature_name (OASISVersion.string_of_version min_version));
      update = update_fail;
      print = (fun s -> s)
    }
  in
  let alpha_features =
    new_field schema
      (OASISFeatures.field_of_stage OASISFeatures.Alpha)
      ~default:[]
      ~feature:OASISFeatures.features
      (comma_separated (value_feature OASISFeatures.Alpha))
      (fun () ->
         s_ "Experimental features in alpha stage \
             (frequent change, may never been shipped).")
      (fun pkg -> pkg.alpha_features)
  in
  let beta_features =
    new_field schema
      (OASISFeatures.field_of_stage OASISFeatures.Beta)
      ~default:[]
      ~feature:OASISFeatures.features
      (comma_separated (value_feature OASISFeatures.Beta))
      (fun () ->
         s_ "Experimental features in beta stage \
             (will ship, under review).")
      (fun pkg -> pkg.beta_features)
  in
  alpha_features, beta_features


let generator =
  let schm = schema in

  let new_field ?quickstart_level ?quickstart_question ?default  =
    new_field schm ?quickstart_level ?quickstart_question ?default
  in
  let new_field_plugin nm ?default ?quickstart_question value hlp sync =
    new_field_plugin schm nm ?default ?quickstart_question value hlp sync
  in
  let name =
    new_field "Name" string_not_empty
      (fun () ->
         s_ "Name of the package.")
      (fun pkg -> pkg.name)
  in
  let version =
    new_field "Version" OASISVersion.value
      (fun () ->
         s_ "Version of the package.")
      (fun pkg -> pkg.version)
  in
  let synopsis =
    new_field "Synopsis" string_not_empty
      (fun () ->
         s_ "Short description of the purpose of this package.")
      (fun pkg -> pkg.synopsis)
  in
  let description =
    new_field "Description"
      ~default:None
      (opt OASISText.value)
      (fun () ->
         s_ "Long description of the package purpose.")
      (fun pkg -> pkg.description)
  in
  let authors =
    new_field "Authors"
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Real people that had contributed to the package.")
      (fun pkg -> pkg.authors)
  in
  let copyrights =
    new_field "Copyrights"
      ~default:[]
      (comma_separated copyright)
      (fun () ->
         s_ "Copyright owners.")
      (fun pkg -> pkg.copyrights)
  in
  let maintainers =
    new_field "Maintainers"
      ~default:[]
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Current maintainers of the package.")
      (fun pkg -> pkg.maintainers)
  in
  let license_file =
    new_field "LicenseFile"
      ~default:None
      (opt file)
      (fun () ->
         s_ "File containing the license.")
      (fun pkg -> pkg.license_file)
  in
  let license =
    new_field "License"
      OASISLicense.value
      ~quickstart_question:(fun () ->
        ExclusiveChoices (OASISLicense.choices ()))
      (fun () ->
         (s_ "DEP-5 license of the package \
              (See [DEP-5](http://dep.debian.net/deps/dep5/#index6h3))."))
      (fun pkg -> pkg.license)
  in
  let ocaml_version =
    new_field "OCamlVersion"
      ~default:None
      (opt OASISVersion.comparator_value)
      (fun () ->
         s_ "Version constraint on OCaml.")
      (fun pkg -> pkg.ocaml_version)
  in
  let findlib_version =
    new_field "FindlibVersion"
      ~default:None
      (opt OASISVersion.comparator_value)
      (fun () ->
         s_ "Version constraint on Finblib.")
      (fun pkg -> pkg.findlib_version)
  in
  let conf_type =
    new_field_plugin "ConfType"
      ~default:(OASISPlugin.builtin `Configure "internal")
      ~quickstart_question:OASISPlugin.Configure.quickstart_question
      `Configure
      OASISPlugin.Configure.value
      (fun () ->
         s_ "Configuration system.")
      (fun pkg -> pkg.conf_type)
  in
  let conf_custom =
    OASISCustom.add_fields schm "Conf"
      (fun () -> s_ "Command to run before configuration.")
      (fun () -> s_ "Command to run after configuration.")
      (fun pkg -> pkg.conf_custom)
  in
  let build_type =
    new_field_plugin "BuildType"
      ~default:(OASISPlugin.builtin `Build "ocamlbuild")
      ~quickstart_question:OASISPlugin.Build.quickstart_question
      `Build
      OASISPlugin.Build.value
      (fun () ->
         s_ "Build system.")
      (fun pkg -> pkg.build_type)
  in
  let build_custom =
    OASISCustom.add_fields schm "Build"
      (fun () -> s_ "Command to run before build.")
      (fun () -> s_ "Command to run after build.")
      (fun pkg -> pkg.build_custom)
  in
  let install_type =
    new_field_plugin "InstallType"
      ~default:(OASISPlugin.builtin `Install "internal")
      ~quickstart_question:OASISPlugin.Install.quickstart_question
      `Install
      OASISPlugin.Install.value
      (fun () ->
         s_ "Install/uninstall system.")
      (fun pkg -> pkg.install_type)
  in
  let install_custom =
    OASISCustom.add_fields schm "Install"
      (fun () -> s_ "Command to run before install.")
      (fun () -> s_ "Command to run after install.")
      (fun pkg -> pkg.install_custom)
  in
  let uninstall_custom =
    OASISCustom.add_fields schm "Uninstall"
      (fun () -> s_ "Command to run before uninstall.")
      (fun () -> s_ "Command to run after uninstall.")
      (fun pkg -> pkg.uninstall_custom)
  in
  let clean_custom =
    OASISCustom.add_fields schm "Clean"
      (fun () -> s_ "Command to run before clean.")
      (fun () -> s_ "Command to run after clean.")
      (fun pkg -> pkg.clean_custom)
  in
  let distclean_custom =
    OASISCustom.add_fields schm "Distclean"
      (fun () -> s_ "Command to run before distclean.")
      (fun () -> s_ "Command to run after distclean.")
      (fun pkg -> pkg.distclean_custom)
  in
  let homepage =
    new_field "Homepage"
      ~default:None
      (opt url)
      (fun () ->
         s_ "URL of the package homepage.")
      (fun pkg -> pkg.homepage)
  in
  let bugreports =
    new_field "BugReports"
      ~default:None
      (opt url)
      (fun () ->
         s_ "URL of the page to report bugs about the package.")
      (fun pkg -> pkg.bugreports)
  in
  let tags =
    new_field "Tags"
      ~default:[]
      (comma_separated string_not_empty)
      (fun () ->
         s_ "List of semantic tags to classify the package.")
      (fun pkg -> pkg.tags)
  in
  let categories =
    new_field "Categories"
      ~default:[]
      categories
      (fun () ->
         s_ "URL(s) describing categories of the package.")
      (fun pkg -> pkg.categories)
  in
  let files_ab =
    new_field "FilesAB"
      ~default:[]
      (* TODO: check that filenames end with .ab *)
      (comma_separated file)
      (fun () -> s_ "Files to generate using environment variable \
                     substitution.")
      (fun pkg -> pkg.files_ab)
  in
  let plugins =
    let quickstart_question () =
      match OASISPlugin.Extra.quickstart_question () with
        | ExclusiveChoices lst ->
          Choices lst
        | Choices _ | Field | Text as q ->
          q
    in
    new_field_plugins schm "Plugins"
      ~default:[]
      ~quickstart_level:Beginner
      ~quickstart_question
      `Extra
      OASISPlugin.Extra.value
      (fun () ->
         s_ "Extra plugins to use.")
      (fun pkg -> pkg.plugins)
  in
  let disable_oasis_section =
    new_field "DisableOASISSection"
      ~default:[]
      ~feature:OASISFeatures.disable_oasis_section
      (comma_separated (expandable file))
      (fun () -> s_ "Files to generate without OASIS section comments or \
                     digest.")
      (fun pkg -> pkg.disable_oasis_section)
  in
  let build_depends =
    OASISBuildSection_intern.build_depends_field schm
      (fun pkg -> [])
  in
  let build_tools =
    OASISBuildSection_intern.build_tools_field schm
      (fun pkg -> [])
  in
  (fun data sections ->
     let plugins = plugins data in
     let conf    = conf_type data in
     let build   = build_type data in
     let install = install_type data in

     (* Generate plugin data *)
     let set_plugin_data generator plugin_data data =
       let rplugin_data = ref plugin_data in
       List.iter
         (fun plg ->
            generator
              (plg :> plugin_kind plugin)
              rplugin_data
              data)
         plugins;
       generator
         (conf :> plugin_kind plugin)
         rplugin_data data;
       generator
         (build :> plugin_kind plugin)
         rplugin_data data;
       generator
         (install :> plugin_kind plugin)
         rplugin_data data;
       !rplugin_data
     in

     (* Plugin data for package *)
     let plugin_data =
       set_plugin_data
         OASISPlugin.generator_package
         []
         data
     in

     (* Fix plugin data for sections, set data from plugin
      * defined at package level
     *)
     let sections =
       List.map
         (fun sct ->
            let knd, cs =
              OASISSection.section_kind_common sct
            in
            let plugin_data =
              set_plugin_data
                (OASISPlugin.generator_section knd)
                cs.cs_plugin_data
                cs.cs_data
            in
            OASISSection.section_common_set
              {cs with cs_plugin_data = plugin_data}
              sct)
         sections
     in

     let oasis_version = oasis_version data in
     let alpha_features = alpha_features data in
     let beta_features = beta_features data in
     if (alpha_features <> [] || beta_features <> []) &&
        (OASISVersion.version_compare
           oasis_version OASISConf.version_short) <> 0 then
       failwithf
         (f_ "You need to use the latest OASISFormat to be able to use \
              fields %s and %s. Change 'OASISFormat: %s' to \
              'OASISFormat: %s'")
         (OASISFeatures.field_of_stage OASISFeatures.Alpha)
         (OASISFeatures.field_of_stage OASISFeatures.Beta)
         (OASISVersion.string_of_version oasis_version)
         (OASISVersion.string_of_version OASISConf.version_short);

     List.fold_right
       add_build_depend
       (build_depends data)
       (List.fold_right
          add_build_tool
          (build_tools data)
          {
            oasis_version          = oasis_version;
            ocaml_version          = ocaml_version data;
            findlib_version        = findlib_version data;
            alpha_features         = alpha_features;
            beta_features          = beta_features;
            name                   = name data;
            version                = version data;
            license                = license data;
            license_file           = license_file data;
            copyrights             = copyrights data;
            maintainers            = maintainers data;
            authors                = authors data;
            homepage               = homepage data;
            bugreports             = bugreports data;
            synopsis               = synopsis data;
            description            = description data;
            tags                   = tags data;
            categories             = categories data;
            conf_type              = conf;
            conf_custom            = conf_custom data;
            build_type             = build;
            build_custom           = build_custom data;
            install_type           = install;
            install_custom         = install_custom data;
            uninstall_custom       = uninstall_custom data;
            clean_custom           = clean_custom data;
            distclean_custom       = distclean_custom data;
            files_ab               = files_ab data;
            plugins                = plugins;
            disable_oasis_section  = disable_oasis_section data;
            sections               = sections;
            schema_data            = data;
            plugin_data            = plugin_data;
          }))
