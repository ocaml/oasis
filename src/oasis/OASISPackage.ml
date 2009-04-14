
(** Package schema and generator 
    @author Sylvain Le Gall
  *)

open OASISSchema;;
open OASISValueParser;;
open OASISTypes;;

let schema, generator =
  let schm =
    schema ()
  in
  let name = 
    new_field schm "name" string_not_empty 
  in
  let version = 
    new_field schm "version" version
  in
  let license_file =
    new_field schm "licensefile" file_exists
  in
  let synopsis =
    new_field schm "synopsis" string_not_empty
  in
  let author =
    new_field schm "author" string_not_empty
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
  in
  let conf_type =
    new_field schm "conftype" 
      ~default:"autobuild"
      string_not_empty
  in
  let build_type =
    new_field schm "buildtype" 
      ~default:"ocamlbuild"
      string_not_empty
  in
  let doc_type =
    new_field schm "doctype" 
      ~default:"none"
      string_not_empty
  in
  let test_type =
    new_field schm "testtype" 
      ~default:"none"
      string_not_empty
  in
  let install_type =
    new_field schm "installtype"
      ~default:"autobuild"
      string_not_empty
  in
  let copyright =
    new_field schm "copyright" 
      ~default:None
      (opt copyright)
  in
  let maintainer =
    new_field schm "maintainer"
      ~default:None
      (opt string_not_empty)
  in
  let homepage =
    new_field schm "homepage" 
      ~default:None
      (opt url)
  in
  let description =
    new_field schm "description"
      ~default:None
      (opt string_not_empty)
  in
  let categories =
    new_field schm "categories"
      ~default:[]
      categories
  in
  let build_depends =
    new_field schm "builddepends" 
      ~default:[]
      build_depends
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
        conf_type     = conf_type wrtr;
        build_type    = build_type wrtr;
        doc_type      = doc_type wrtr;
        test_type     = test_type wrtr;
        install_type  = install_type wrtr;
        libraries     = libs;
        executables   = execs;
        flags         = flags;
        extra         = extra wrtr;
      })
;;
