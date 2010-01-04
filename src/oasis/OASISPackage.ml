
(** Package schema and generator 
    @author Sylvain Le Gall
  *)

open OASISSchema;;
open OASISValueParser;;
open OASISTypes;;
open CommonGettext;;
open PropList.Field;;

let schema, generator =
  let schm =
    schema "Package"
  in
  let oasis_version = 
    new_field schm "OASISFormat" 
      (fun str -> 
         match str with 
           | "1.0" -> "1.0"
           | str ->
               failwith 
                 (Printf.sprintf 
                    "OASIS format version '%s' is not supported"
                    str))
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
  let license_file =
    new_field schm "licensefile" file
      (fun () -> 
         s_ "File containing license.");
  in
  let synopsis =
    new_field schm "Synopsis" string_not_empty
      (fun () ->
         s_ "Short description of the purpose of this package.")
  in
  let authors =
    new_field schm "Authors" 
      comma_separated
      (fun () ->
         s_ "Real person that has contributed to the package.")
  in
  let license =
    new_field schm "License"
      (fun str ->
         try 
           choices
             (s_ "license")
             [
               "GPL", `GPL;
               "LGPL", `LGPL;
               "BSD3", `BSD3;
               "BSD4", `BSD4;
               "PUBLICDOMAIN", `PublicDomain;
               "LGPL-LINK-EXN", `LGPL_link_exn;
             ]
             str
         with _ ->
           begin
             try
               `Other (url str)
             with _ ->
               failwith 
                 (Printf.sprintf 
                    (f_ "'%s' is not an URL or a common license name")
                    str)
           end)
      (fun () ->
         s_ "License type of the package.")
  in
  let ocaml_version =
    new_field schm "OCamlVersion"
      ~default:None
      (opt version_constraint)
      (fun () -> 
         s_ "Version constraint on OCaml.")
  in
  let conf_type =
    new_field schm "ConfType" 
      ~default:"internal"
      string_not_empty
      (fun () -> 
         s_ "Configuration system.")
  in
  let build_type =
    new_field schm "BuildType" 
      ~default:"ocamlbuild"
      string_not_empty
      (fun () -> 
         s_ "Build system.")
  in
  let install_type =
    new_field schm "InstallType"
      ~default:"internal"
      string_not_empty
      (fun () -> 
         s_ "Install/uninstall system.")
  in
  let copyrights =
    new_field schm "Copyrights" 
      ~default:[]
      (fun str ->
         List.map 
           copyright
           (comma_separated str))
      (fun () ->
         s_ "Copyright owners.")
  in
  let maintainers =
    new_field schm "Maintainers"
      ~default:[]
      comma_separated
      (fun () -> 
         s_ "Current maintainers of the package")
  in
  let homepage =
    new_field schm "Homepage" 
      ~default:None
      (opt url)
      (fun () -> 
         s_ "URL of the package homepage.")
  in
  let description =
    new_field schm "Description"
      ~default:None
      (opt string_not_empty)
      (fun () -> 
         s_ "Long description of the package purpose.")
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
      (fun str ->
         List.map 
           file 
           (comma_separated str))
      (fun () -> 
         s_ "Files to generate using environment variable substitution.")
  in
  let plugins =
    new_field schm "Plugins"
      ~default:[]
      comma_separated
      (fun () -> 
         s_ "Extra plugins to use")
  in
  let build_depends, build_tools =
    OASISUtils.depends_field schm
  in
    schm,
    (fun data libs execs flags src_repos tests ->
      {
        oasis_version = oasis_version data;
        ocaml_version = ocaml_version data;
        name          = name data;
        version       = version data;
        license       = license data;
        license_file  = license_file data;
        copyrights    = copyrights data;
        maintainers   = maintainers data;
        authors       = authors data;
        homepage      = homepage data;
        synopsis      = synopsis data;
        description   = description data;
        categories    = categories data;
        build_depends = build_depends data;
        build_tools   = build_tools data;
        conf_type     = conf_type data;
        build_type    = build_type data;
        install_type  = install_type data;
        files_ab      = files_ab data;
        plugins       = plugins data;
        libraries     = libs;
        executables   = execs;
        flags         = flags;
        src_repos     = src_repos;
        tests         = tests;
        schema_data   = data;
      })
;;
