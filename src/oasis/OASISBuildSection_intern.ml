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

(** Build section internal functions
    @author Sylvain Le Gall
  *)

open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext
open OASISTypes

let build_depends_field schm = 
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
    (fun () -> s_ "Dependencies on findlib packages, including internal findlib packages.")

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

let build_tools_field schm =
  new_field schm "BuildTools"
    ~default:[]
    build_tools_value
    (fun () -> s_ "Tools required to compile, including internal executables.")

let build_install_data_fields schm = 
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (fun () -> s_ "Set if the section should be built.")
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (fun () -> s_ "Set if the section should be distributed.")
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
  in
    build, install, data_files

let section_fields nm comp_dflt schm = 
  let path =
    new_field schm "Path" 
      directory
      (fun () -> s_ "Directory containing the section")
  in
  let build, install, data_files = 
    build_install_data_fields schm
  in
  let build_depends =
    build_depends_field schm
  in
  let build_tools =
    build_tools_field schm
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      (choices
         (fun () -> s_ "compiled object")
         ["byte", Byte; "native", Native; "best", Best])
      (fun () -> 
         s_ "Define the compilation type of the section: byte, native or best")
  in
  let c_sources = 
    new_field schm "CSources"
      ~default:[]
      files
      (fun () -> s_ "C source files.")
  in
  let ccopt = 
    new_field_conditional schm "CCOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-ccopt arguments to use when building.")
  in
  let cclib = 
    new_field_conditional schm "CCLib"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-cclib arguments to use when building.")
  in
  let dlllib = 
    new_field_conditional schm "DllLib"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-dlllib arguments to use when building.")
  in
  let dllpath = 
    new_field_conditional schm "DllPath"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-dllpath arguments to use when building.")
  in
  let byteopt = 
    new_field_conditional schm "ByteOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "ocamlc arguments to use when building.")
  in
  let nativeopt = 
    new_field_conditional schm "NativeOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "ocamlopt arguments to use when building.")
  in
    (fun nm data ->
       {
         bs_build           = build data;
         bs_install         = install data;
         bs_path            = path data;
         bs_compiled_object = compiled_object data;
         bs_build_depends   = build_depends data;
         bs_build_tools     = build_tools data;
         bs_c_sources       = c_sources data;
         bs_data_files      = data_files data;
         bs_ccopt           = ccopt data;
         bs_cclib           = cclib data;
         bs_dlllib          = dlllib data;
         bs_dllpath         = dllpath data;
         bs_byteopt         = byteopt data;
         bs_nativeopt       = nativeopt data;
       })

