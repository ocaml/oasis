
(** Package schema and generator 
    @author Sylvain Le Gall
  *)

open OASISSchema;;
open OASISValueParser;;
open OASISTypes;;
open CommonGettext;;

let schema, generator =
  let schm =
    schema "package"
  in
  let oasis_version = 
    new_field schm "OASISFormat" 
      (fun ctxt str -> 
         match str with 
           | "1.0" -> "1.0"
           | str ->
               failwith 
                 (Printf.sprintf 
                    "OASIS format version '%s' is not supported"
                    str))
      (s_ "OASIS format version used to write file _oasis.")
  in
  let name = 
    new_field schm "name" string_not_empty 
      (s_ "Name of the package.")
  in
  let version = 
    new_field schm "version" version
      (s_ "Version of the package.")
  in
  let license_file =
    new_field schm "licensefile" file_exists
      (s_ "File containing license.");
  in
  let synopsis =
    new_field schm "synopsis" string_not_empty
      (s_ "Short description of the purpose of this package.")
  in
  let authors =
    new_field schm "authors" 
      comma_separated
      (s_ "Real person that has contributed to the package.")
  in
  let license =
    new_field schm "license"
      (fun ctxt str ->
         match String.uppercase str with 
           | "GPL"   -> `GPL
           | "LGPL"  -> `LGPL
           | "BSD3"  -> `BSD3
           | "BSD4"  -> `BSD4
           | "PUBLICDOMAIN" -> `PublicDomain
           | "LGPL-LINK-EXN" -> `LGPL_link_exn
           | _ -> 
               (try
                  `Other (url ctxt str)
                with _ ->
                  failwith (Printf.sprintf 
                              "'%s' is not an URL or a common license name"
                              str)))
      (s_ "License type of the package.")
  in
  let ocaml_version =
    new_field schm "OCamlVersion"
      ~default:None
      (opt version_constraint)
      (s_ "Version constraint on OCaml.")
  in
  let conf_type =
    new_field schm "conftype" 
      ~default:"internal"
      string_not_empty
      (s_ "Configuration system.")
  in
  let build_type =
    new_field schm "buildtype" 
      ~default:"ocamlbuild"
      string_not_empty
      (s_ "Build system.")
  in
  let doc_type =
    new_field schm "doctype" 
      ~default:"none"
      string_not_empty
      (s_ "Documentation build system.")
  in
  let test_type =
    new_field schm "testtype" 
      ~default:"none"
      string_not_empty
      (s_ "Test suite system.")
  in
  let install_type =
    new_field schm "installtype"
      ~default:"internal"
      string_not_empty
      (s_ "Install/uninstall system.")
  in
  let copyrights =
    new_field schm "copyrights" 
      ~default:[]
      (fun ctxt str ->
         List.map 
           (copyright ctxt)
           (comma_separated ctxt str))
      (s_ "Copyright owners.")
  in
  let maintainers =
    new_field schm "maintainers"
      ~default:[]
      comma_separated
      (s_ "Current maintainers of the package")
  in
  let homepage =
    new_field schm "homepage" 
      ~default:None
      (opt url)
      (s_ "URL of the package homepage.")
  in
  let description =
    new_field schm "description"
      ~default:None
      (opt string_not_empty)
      (s_ "Long description of the package purpose.")
  in
  let categories =
    new_field schm "categories"
      ~default:[]
      categories
      (s_ "URL(s) describing categories of the package.")
  in
  let files_ab =
    new_field schm "filesab"
      ~default:[]
      (fun ctxt str ->
         List.map 
           (file_exists ctxt) 
           (comma_separated ctxt str))
      (s_ "Files to generate using environment variable substitution.")
  in
  let plugins =
    new_field schm "Plugins"
      ~default:[]
      comma_separated
      (s_ "Extra plugins to use")
  in
  let build_depends, build_tools =
    OASISUtils.depends_field schm
  in
    schm,
    (fun wrtr libs execs flags ->
      {
        oasis_version = oasis_version wrtr;
        ocaml_version = ocaml_version wrtr;
        name          = name wrtr;
        version       = version wrtr;
        license       = license wrtr;
        license_file  = license_file wrtr;
        copyrights    = copyrights wrtr;
        maintainers   = maintainers wrtr;
        authors       = authors wrtr;
        homepage      = homepage wrtr;
        synopsis      = synopsis wrtr;
        description   = description wrtr;
        categories    = categories wrtr;
        build_depends = build_depends wrtr;
        build_tools   = build_tools wrtr;
        conf_type     = conf_type wrtr;
        build_type    = build_type wrtr;
        doc_type      = doc_type wrtr;
        test_type     = test_type wrtr;
        install_type  = install_type wrtr;
        files_ab      = files_ab wrtr;
        plugins       = plugins wrtr;
        libraries     = libs;
        executables   = execs;
        flags         = flags;
        schema_data   = wrtr;
      })
;;
