
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
  let author =
    new_field schm "author" string_not_empty
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
  let conf_type =
    new_field schm "conftype" 
      ~default:"autobuild"
      string_not_empty
      (s_ "Configuration system.")
  in
  let build_type =
    new_field schm "buildtype" 
      ~default:"ocamlbuild"
      string_not_empty
      (s_ "Build system.")
  in
  let build_tools =
    new_field schm "buildtools"
      ~default:[]
      comma_separated
      (s_ "Executables require to compile.")
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
      ~default:"autobuild"
      string_not_empty
      (s_ "Install/uninstall system.")
  in
  let copyright =
    new_field schm "copyright" 
      ~default:None
      (opt copyright)
      (s_ "Copyright owners.")
  in
  let maintainer =
    new_field schm "maintainer"
      ~default:None
      (opt string_not_empty)
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
  let build_depends =
    new_field schm "builddepends" 
      ~default:[]
      build_depends
      (s_ "Dependencies on external libraries (findlib).")
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
    schm,
    (fun wrtr libs execs flags ->
      {
        name          = name wrtr;
        version       = version wrtr;
        license       = license wrtr;
        license_file  = license_file wrtr;
        copyright     = copyright wrtr;
        maintainer    = maintainer wrtr;
        author        = author wrtr;
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
        libraries     = libs;
        executables   = execs;
        flags         = flags;
        extra         = extra wrtr;
      })
;;
