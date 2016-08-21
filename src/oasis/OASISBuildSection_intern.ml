(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


(** Build section internal functions
    @author Sylvain Le Gall
*)


open OASISSchema_intern
open OASISValues
open OASISGettext
open OASISTypes


let build_depends_field schm sync =
  new_field schm "BuildDepends"
    ~default:[]
    (let base_value =
       comma_separated
         (with_optional_parentheses
            findlib_full
            OASISVersion.comparator_value)
     in
     {
       parse =
         (fun ~ctxt str ->
            List.map
              (fun (pkg, ver_constr_opt) ->
                 FindlibPackage (pkg, ver_constr_opt))
              (base_value.parse ~ctxt str));

       update =
         List.append;

       print =
         (fun lst ->
            base_value.print
              (List.map
                 (function
                   | FindlibPackage (nm, ver) -> (nm, ver)
                   | InternalLibrary nm -> (nm, None))
                 lst));
     })
    (fun () ->
       s_ "Dependencies on findlib packages, including internal \
           findlib packages.")
    sync


let build_tools_value =
  let base =
    comma_separated file
  in
  {
    parse =
      (fun ~ctxt str ->
         List.map
           (fun s -> ExternalTool s)
           (base.parse ~ctxt str));

    update =
      List.append;

    print =
      (fun lst ->
         base.print
           (List.map
              (function
                | InternalExecutable nm
                | ExternalTool nm -> nm)
              lst))
  }


let build_tools_field schm sync =
  new_field schm "BuildTools"
    ~default:[]
    build_tools_value
    (fun () -> s_ "Tools required to compile, including internal executables.")
    sync


let build_install_data_fields
    ?default_cond
    ?(default=true)
    schm
    sync_build
    sync_install
    sync_datafiles =
  let build =
    new_field_conditional schm "Build"
      ?default_cond
      ~default
      boolean
      (fun () -> s_ "Set if the section should be built.")
      sync_build
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (fun () -> s_ "Set if the section should be distributed.")
      sync_install
  in
  let data_files =
    new_field schm "DataFiles"
      ~default:[]
      (comma_separated
         (with_optional_parentheses
            file_glob
            (expandable directory)))
      (fun () ->
         s_ "Comma separated list of files to be installed for run-time. \
             ([see here](#data-files))")
      sync_datafiles
  in
  build, install, data_files

let source_patterns_fields schm default_intf sync_intf default_impl sync_impl =
  let value_source_pattern =
    let open OASISValues in
    {
      parse = (fun ~ctxt:_ str -> OASISSourcePatterns.parse str);
      update = update_fail;
      print = OASISSourcePatterns.to_string;
    }
  in
  let value_source_patterns =
    OASISValues.comma_separated value_source_pattern
  in
  let interface_patterns =
    new_field schm "InterfacePatterns"
      ~default:default_intf
      ~feature:OASISFeatures.source_patterns
      value_source_patterns
      (fun () -> s_ "Patterns to use for locating source files.")
      sync_intf
  in
  let implementation_patterns =
    new_field schm "ImplementationPatterns"
      ~default:default_impl
      ~feature:OASISFeatures.source_patterns
      value_source_patterns
      (fun () -> s_ "Patterns to use for locating source files.")
      sync_impl
  in
  interface_patterns, implementation_patterns


let section_fields _ comp_dflt schm sync =
  let path =
    new_field schm "Path"
      directory
      (fun () -> s_ "Directory containing the section")
      (fun pkg -> (sync pkg).bs_path)
  in
  let build, install, data_files =
    build_install_data_fields schm
      (fun pkg -> (sync pkg).bs_build)
      (fun pkg -> (sync pkg).bs_install)
      (fun pkg -> (sync pkg).bs_data_files)
  in
  let findlib_extra_files =
    new_field schm "FindlibExtraFiles"
      ~default:[]
      ~feature:OASISFeatures.findlib_extra_files
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Comma separated list of extra files to be installed with \
             ocamlfind.")
      (fun pkg -> (sync pkg).bs_findlib_extra_files)
  in
  let build_depends =
    build_depends_field schm
      (fun pkg -> (sync pkg).bs_build_depends)
  in
  let build_tools =
    build_tools_field schm
      (fun pkg -> (sync pkg).bs_build_tools)
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      (choices
         (fun () -> s_ "compiled object")
         ["byte", Byte; "native", Native; "best", Best])
      (fun () ->
         s_ "Define the compilation type of the section: byte, native or best")
      (fun pkg -> (sync pkg).bs_compiled_object)
  in
  let interface_patterns, implementation_patterns =
    source_patterns_fields schm
      OASISSourcePatterns.interface
      (fun pkg -> (sync pkg).bs_interface_patterns)
      OASISSourcePatterns.implementation
      (fun pkg -> (sync pkg).bs_implementation_patterns)
  in
  let c_sources =
    new_field schm "CSources"
      ~default:[]
      files
      (fun () -> s_ "C source files.")
      (fun pkg -> (sync pkg).bs_c_sources)
  in
  let ccopt =
    new_field_conditional schm "CCOpt"
      ~default:[]
      command_line_options
      (fun () -> s_ "-ccopt arguments to use when building.")
      (fun pkg -> (sync pkg).bs_ccopt)
  in
  let cclib =
    new_field_conditional schm "CCLib"
      ~default:[]
      command_line_options
      (fun () -> s_ "-cclib arguments to use when building.")
      (fun pkg -> (sync pkg).bs_cclib)
  in
  let dlllib =
    new_field_conditional schm "DllLib"
      ~default:[]
      command_line_options
      (fun () -> s_ "-dlllib arguments to use when building.")
      (fun pkg -> (sync pkg).bs_dlllib)
  in
  let dllpath =
    new_field_conditional schm "DllPath"
      ~default:[]
      command_line_options
      (fun () -> s_ "-dllpath arguments to use when building.")
      (fun pkg -> (sync pkg).bs_dllpath)
  in
  let byteopt =
    new_field_conditional schm "ByteOpt"
      ~default:[]
      command_line_options
      (fun () -> s_ "ocamlc arguments to use when building.")
      (fun pkg -> (sync pkg).bs_byteopt)
  in
  let nativeopt =
    new_field_conditional schm "NativeOpt"
      ~default:[]
      command_line_options
      (fun () -> s_ "ocamlopt arguments to use when building.")
      (fun pkg -> (sync pkg).bs_nativeopt)
  in
  (fun _ data ->
     {
       bs_build                   = build data;
       bs_install                 = install data;
       bs_path                    = path data;
       bs_compiled_object         = compiled_object data;
       bs_build_depends           = build_depends data;
       bs_build_tools             = build_tools data;
       bs_interface_patterns      = interface_patterns data;
       bs_implementation_patterns = implementation_patterns data;
       bs_c_sources               = c_sources data;
       bs_data_files              = data_files data;
       bs_findlib_extra_files     = findlib_extra_files data;
       bs_ccopt                   = ccopt data;
       bs_cclib                   = cclib data;
       bs_dlllib                  = dlllib data;
       bs_dllpath                 = dllpath data;
       bs_byteopt                 = byteopt data;
       bs_nativeopt               = nativeopt data;
     })
