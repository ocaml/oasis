(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Package schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISValues
open OASISUtils
open OASISSchema
open OASISGettext
open OASISExpr

let mod_build_depends f pkg = 
  {pkg with
       sections = 
         List.map
           (function
              | Library (cs, bs, lib) ->
                  Library (cs, f bs, lib)
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
                                 test_tools = 
                                   build_tool :: test.test_tools})
                  | Doc (cs, doc) ->
                      Doc (cs,
                           {doc with 
                                doc_build_tools = 
                                  build_tool :: doc.doc_build_tools})
                  | Library _ | Executable _ | Flag _ | SrcRepo _ as sct ->
                      sct)
               pkg.sections}

let schema, generator =
  let schm =
    schema "Package"
  in
  let oasis_version = 
    let current_version =
      OASISVersion.version_of_string "0.1"
    in
    let extra_supported_versions =
      []
    in
      new_field schm "OASISFormat" 
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
                   failwithf1
                     (f_ "OASIS format version '%s' is not supported.")
                     str;
                 v);
          update = update_fail;
          print = OASISVersion.value.print;
        }
        (fun () ->
           s_ "OASIS format version used to write file `_oasis`.")
  in
  let name = 
    new_field schm "Name" string_not_empty 
      (fun () ->
         s_ "Name of the package.")
  in
  let version = 
    new_field schm "Version" OASISVersion.value
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
         s_ "File containing the license.");
  in
  let authors =
    new_field schm "Authors" 
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Real people that had contributed to the package.")
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
         s_ "Current maintainers of the package.")
  in
  let license =
    new_field schm "License"
      OASISLicense.value
      (fun () ->
         (s_ "License type of the package.")^
         (OASISLicense.help ()))
  in
  let ocaml_version =
    new_field schm "OCamlVersion"
      ~default:None
      (opt OASISVersion.comparator_value)
      (fun () -> 
         s_ "Version constraint on OCaml.")
  in
  let findlib_version =
    new_field schm "FindlibVersion"
      ~default:None
      (opt OASISVersion.comparator_value)
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
  let conf_custom = 
    OASISCustom.add_fields schm "Conf"
      (fun () -> s_ "Command to run before configuration.")
      (fun () -> s_ "Command to run after configuration.")
  in
  let build_type =
    new_field schm "BuildType" 
      ~default:(OASISPlugin.builtin "ocamlbuild")
      OASISPlugin.Build.value
      (fun () -> 
         s_ "Build system.")
  in
  let build_custom = 
    OASISCustom.add_fields schm "Build"
      (fun () -> s_ "Command to run before build.")
      (fun () -> s_ "Command to run after build.")
  in
  let install_type =
    new_field schm "InstallType"
      ~default:(OASISPlugin.builtin "internal")
      OASISPlugin.Install.value
      (fun () -> 
         s_ "Install/uninstall system.")
  in
  let install_custom = 
    OASISCustom.add_fields schm "Install"
      (fun () -> s_ "Command to run before install.")
      (fun () -> s_ "Command to run after install.")
  in
  let uninstall_custom = 
    OASISCustom.add_fields schm "Uninstall"
      (fun () -> s_ "Command to run before uninstall.")
      (fun () -> s_ "Command to run after uninstall.")
  in
  let clean_custom = 
    OASISCustom.add_fields schm "Clean"
      (fun () -> s_ "Command to run before clean.")
      (fun () -> s_ "Command to run after clean.")
  in
  let distclean_custom = 
    OASISCustom.add_fields schm "Distclean"
      (fun () -> s_ "Command to run before distclean.")
      (fun () -> s_ "Command to run after distclean.")
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
         s_ "Extra plugins to use.")
  in
  let build_depends =
    OASISBuildSection_intern.build_depends_field schm
  in
  let build_tools =
    OASISBuildSection_intern.build_tools_field schm
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
              oasis_version    = oasis_version data;
              ocaml_version    = ocaml_version data;
              findlib_version  = findlib_version data;
              name             = name data;
              version          = version data;
              license          = license data;
              license_file     = license_file data;
              copyrights       = copyrights data;
              maintainers      = maintainers data;
              authors          = authors data;
              homepage         = homepage data;
              synopsis         = synopsis data;
              description      = description data;
              categories       = categories data;
              conf_type        = conf_type data;
              conf_custom      = conf_custom data;
              build_type       = build_type data;
              build_custom     = build_custom data;
              install_type     = install_type data;
              install_custom   = install_custom data;
              uninstall_custom = uninstall_custom data;
              clean_custom     = clean_custom data;
              distclean_custom = distclean_custom data;
              files_ab         = files_ab data;
              plugins          = plugins data;
              sections         = sections;
              schema_data      = data;
            }))
