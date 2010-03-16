
(** Package schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISValues
open OASISUtils
open OASISSchema
open OASISGettext

let mod_build_depends f pkg = 
  {pkg with
       sections = 
         List.map
           (function
              | Library (cs, bs, lib) ->
                  Library (cs, f bs, lib)
              | Executable (cs, bs, exec) ->
                  Executable (cs, f bs, exec)
              | Test _ | Flag _ | SrcRepo _ as sct ->
                  sct)
           pkg.sections}

let add_build_depend build_depend pkg =
  mod_build_depends
    (fun bs ->
       {bs with bs_build_depends = build_depend :: bs.bs_build_depends})
    pkg

let add_build_tool ?(no_test=false) ?(condition=[EBool true, true]) build_tool pkg =
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
                                 test_build_tools = 
                                   build_tool :: test.test_build_tools})
                  | Library _ | Executable _ | Flag _ | SrcRepo _ as sct ->
                      sct)
               pkg.sections}

let schema, generator =
  let schm =
    schema "Package"
  in
  let oasis_version = 
    let current_version =
      OASISVersion.version_of_string "1.0"
    in
    let extra_supported_versions =
      []
    in
      new_field schm "OASISFormat" 
        ~quickstart_level:(NoChoice current_version)
        {
          parse = 
            (fun str ->
               let v = 
                 version.parse str
               in
                 if not 
                      (List.mem 
                         v 
                         (current_version :: extra_supported_versions)) then
                   failwith 
                     (Printf.sprintf 
                        "OASIS format version '%s' is not supported"
                        str);
                 v);
          update = update_fail;
          print = version.print;
        }
        (fun () ->
           s_ "OASIS format version used to write file _oasis.")
  in
  let name = 
    new_field schm "Name" string_not_empty 
      (fun () ->
         s_ "Name of the package.")
  in
  let version = 
    new_field schm "Version" version
      (fun () ->
         s_ "Version of the package.")
  in
  let synopsis =
    new_field schm "Synopsis" string_not_empty
      (fun () ->
         s_ "Short description of the purpose of this package.")
  in
  let description =
    new_field schm "Description"
      ~default:None
      (opt string_not_empty)
      (fun () -> 
         s_ "Long description of the package purpose.")
  in
  let license_file =
    new_field schm "LicenseFile" 
      ~default:None
      (opt file)
      (fun () -> 
         s_ "File containing license.");
  in
  let authors =
    new_field schm "Authors" 
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Real person that has contributed to the package.")
  in
  let copyrights =
    new_field schm "Copyrights" 
      ~default:[]
      (comma_separated copyright)
      (fun () ->
         s_ "Copyright owners.")
  in
  let maintainers =
    new_field schm "Maintainers"
      ~default:[]
      (comma_separated string_not_empty)
      (fun () -> 
         s_ "Current maintainers of the package")
  in
  let license =
    new_field schm "License"
      (
        let std_licenses = 
          [
            "GPL", GPL;
            "LGPL", LGPL;
            "BSD3", BSD3;
            "BSD4", BSD4;
            "PUBLICDOMAIN", PublicDomain;
            "LGPL-LINK-EXN", LGPL_link_exn;
          ]
        in
        let base_value = 
          choices (fun () -> s_ "license") std_licenses
        in
          {
            parse =
              (fun str ->
                 try 
                   base_value.parse str
                 with _ ->
                   begin
                     try
                       OtherLicense (url.parse str)
                     with _ ->
                       failwith 
                         (Printf.sprintf 
                            (f_ "'%s' is not an URL or a common license name (%s)")
                            str
                            (String.concat ", " (List.map fst std_licenses)))
                   end);
            update = base_value.update;
            print = 
              (function
                 | OtherLicense v -> url.print v
                 | v -> base_value.print v);
          })
      (fun () ->
         s_ "License type of the package.")
  in
  let ocaml_version =
    new_field schm "OCamlVersion"
      ~default:None
      (opt version_comparator)
      (fun () -> 
         s_ "Version constraint on OCaml.")
  in
  let findlib_version =
    new_field schm "FindlibVersion"
      ~default:None
      (opt version_comparator)
      (fun () ->
         s_ "Version constraint on Finblib.")
  in
  let conf_type =
    new_field schm "ConfType" 
      ~default:(OASISPlugin.builtin "internal")
      OASISPlugin.Configure.value
      (fun () -> 
         s_ "Configuration system.")
  in
  let build_type =
    new_field schm "BuildType" 
      ~default:(OASISPlugin.builtin "ocamlbuild")
      OASISPlugin.Build.value
      (fun () -> 
         s_ "Build system.")
  in
  let install_type =
    new_field schm "InstallType"
      ~default:(OASISPlugin.builtin "internal")
      OASISPlugin.Install.value
      (fun () -> 
         s_ "Install/uninstall system.")
  in
  let homepage =
    new_field schm "Homepage" 
      ~default:None
      (opt url)
      (fun () -> 
         s_ "URL of the package homepage.")
  in
  let categories =
    new_field schm "Categories"
      ~default:[]
      categories
      (fun () ->
         s_ "URL(s) describing categories of the package.")
  in
  let files_ab =
    new_field schm "FilesAB"
      ~default:[]
      (* TODO: check that filenames end with .ab *)
      (comma_separated file)
      (fun () -> 
         s_ "Files to generate using environment variable substitution.")
  in
  let plugins =
    new_field schm "Plugins"
      ~default:[]
      (comma_separated OASISPlugin.Extra.value)
      (fun () -> 
         s_ "Extra plugins to use")
  in
  let build_depends =
    OASISBuildSection.build_depends_field schm
  in
  let build_tools =
    OASISBuildSection.build_tools_field schm
  in
    schm,
    (fun data sections ->
       List.fold_right
         add_build_depend 
         (build_depends data)
         (List.fold_right
            add_build_tool
            (build_tools data)
            {
              oasis_version   = oasis_version data;
              ocaml_version   = ocaml_version data;
              findlib_version = findlib_version data;
              name            = name data;
              version         = version data;
              license         = license data;
              license_file    = license_file data;
              copyrights      = copyrights data;
              maintainers     = maintainers data;
              authors         = authors data;
              homepage        = homepage data;
              synopsis        = synopsis data;
              description     = description data;
              categories      = categories data;
              conf_type       = conf_type data;
              build_type      = build_type data;
              install_type    = install_type data;
              files_ab        = files_ab data;
              plugins         = plugins data;
              sections        = sections;
              schema_data     = data;
            }))
