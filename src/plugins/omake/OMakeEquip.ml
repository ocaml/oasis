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

(** Put OMakeroot, OMakefile etc. in place.
    @author Gerd Stolpmann
  *)

(* TODO:

    - interpret "build" flag
    - pass down compiler flags
    - library with -pack
    - fix-up "omake clean"
    - "omake install"
    - support for objects
    - support for documents
    - OASIS_modify_XXX
 *)

open OASISPlugin
open OASISTypes
open OMakeFormat
open Printf

type dir =
    { dir_path : string;
      dir_top : bool;
      dir_sub : string list;
      dir_build : om_entry list;
    }

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

type dir_map = dir StrMap.t

let new_dir path =
  { dir_path = path;
    dir_top = false;
    dir_sub = [];
    dir_build = [];
  }

let new_dir_map() =
  let top_dir = new_dir OASISUnixPath.current_dir_name in
  let top_dir = { top_dir with dir_top = true } in
  StrMap.add top_dir.dir_path top_dir StrMap.empty

let rec establish map dir =
  if dir.dir_path = "/" then (* CHECK: can we really run into this? *)
    failwith "Absolute paths not supported here";
  if StrMap.mem dir.dir_path map then
    map
  else
    let container = OASISUnixPath.dirname dir.dir_path in
    let cont_dir = new_dir container in
    let map1 = establish map cont_dir in
    let cont_dir1 = StrMap.find container map1 in
    let cont_dir2 =
      { cont_dir1 with dir_sub = dir.dir_path :: cont_dir1.dir_sub } in
    let map2 = StrMap.add container cont_dir2 map1 in
    StrMap.add dir.dir_path dir map2


let establish_in map bs module_includes =
  (* Also create OMakefile in all directories storing the module files: *)
  StrSet.fold
    (fun include_dir acc ->
       establish map (new_dir (OASISUnixPath.concat bs.bs_path include_dir))
    )
    module_includes
    map


let gen_getvar name =
  Expression (sprintf "$(OASIS_getvar %s)" name)


let fixup_module_case dir name =
  let name_cap = OASISUnixPath.capitalize_file name in
  let name_uncap = OASISUnixPath.uncapitalize_file name in
  try
    let (name_fixed,_) =
      List.find
        (fun (name,ext) ->
           let file = OASISUnixPath.concat dir (name ^ ext) in
           Sys.file_exists
             (OASISHostPath.of_unix file)
        )
        [ name_cap, ".ml";
          name_cap, ".mli";
          name_cap, ".mly";
          name_cap, ".mll";
          name_uncap, ".ml";
          name_uncap, ".mli";
          name_uncap, ".mly";
          name_uncap, ".mll";
        ] in
    name_fixed
  with
    | Not_found ->
        name


let strset_flatten l =
  List.fold_left StrSet.union StrSet.empty l


let get_lib_includes pkg bs =
  (* only the direct includes, not the indirect ones *)
  strset_flatten
    (List.map
       (function
         | FindlibPackage _ -> StrSet.empty
         | InternalLibrary sect_name ->
             let sect =
               try OASISSection.section_find (`Library,sect_name) pkg.sections
               with Not_found ->
                 failwith (sprintf "Cannot find section: %s" sect_name) in
             ( match sect with
                 | Library(_,sect_bs,_) ->
                     StrSet.singleton
                       (OASISUnixPath.make_relative
                          bs.bs_path
                          sect_bs.bs_path
                       )
                 | _ ->
                     StrSet.empty
             )
       )
       bs.bs_build_depends
    )


let get_lib_deps ?(transitive=false) pkg bs =
  (* transitive=false: only the direct dependencies, not the indirect ones.
     transitive=true: the indirect ones too
   *)
  let trans_map =
    if transitive then
      OASISBuildSection.transitive_build_depends pkg
    else
      OASISSection.MapSection.empty in
  strset_flatten
    (List.map
       (function
         | FindlibPackage _ -> StrSet.empty
         | InternalLibrary sect_name ->
             let sect =
               try OASISSection.section_find (`Library,sect_name) pkg.sections
               with Not_found ->
                 failwith (sprintf "Cannot find section: %s" sect_name) in
             let deps =
               if transitive then
                 [InternalLibrary sect_name] @
                   OASISSection.MapSection.find sect trans_map
               else
                 [InternalLibrary sect_name] in
             strset_flatten
               (List.map
                  (function
                    | FindlibPackage _ -> StrSet.empty
                    | InternalLibrary sect_name2 ->
                        let sect2 =
                          try OASISSection.section_find
                                (`Library,sect_name2) pkg.sections
                          with Not_found ->
                            failwith
                              (sprintf "Cannot find section: %s" sect_name2) in
                        ( match sect2 with
                            | Library(_,sect_bs,_) ->
                                StrSet.singleton
                                  (OASISUnixPath.concat
                                     (OASISUnixPath.make_relative
                                        bs.bs_path
                                        sect_bs.bs_path
                                     )
                                     sect_name2
                                  )
                            | _ ->
                                StrSet.empty
                        )
                  )
                  deps
               )
       )
       bs.bs_build_depends
    )


let get_ocamlpacks ?(transitive=false) pkg bs =
  let trans_map =
    if transitive then
      OASISBuildSection.transitive_build_depends pkg
    else
      OASISSection.MapSection.empty in
  List.flatten
    (List.map
       (function
         | FindlibPackage(flib,_) -> [flib]
         | InternalLibrary sect_name when transitive ->
             let sect =
               try OASISSection.section_find (`Library,sect_name) pkg.sections
               with Not_found ->
                 failwith (sprintf "Cannot find section: %s" sect_name) in
             let deps =
               OASISSection.MapSection.find sect trans_map in
             List.flatten
               (List.map
                  (function
                    | FindlibPackage(flib,_) -> [flib]
                    | InternalLibrary _ -> []
                  )
                  deps
               )
         | InternalLibrary _ -> []
       )
       bs.bs_build_depends
    )

let get_c_object name =
  OASISUnixPath.chop_extension name ^ ".o"


let add_library ctx pkg map cs bs lib =
  (* CHECK: what if bs.bs_path contains .. path elements? What if module names
     do so?
   *)
  let map = establish map (new_dir bs.bs_path) in
  let lib_includes = get_lib_includes pkg bs in
  let lib_deps = get_lib_deps pkg bs in
  let module_includes =
    List.fold_left
      (fun acc m ->
         let d = OASISUnixPath.dirname m in
         if OASISUnixPath.is_current_dir d then
           acc
         else
           StrSet.add d acc
      )
      StrSet.empty
      (lib.lib_modules @ lib.lib_internal_modules) in
  let ocaml_includes =
    StrSet.union lib_includes module_includes in
  let ocamlpacks =
    get_ocamlpacks pkg bs in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      ( match bs.bs_compiled_object with
          | Best ->
              Nop
          | Byte ->
              Set_string(false, "NATIVE_ENABLED", Literal "false")
          | Native ->
              Set_string(false, "BYTE_ENABLED", Literal "false")
      );
      Set_array(false, "MODULES",
                ( List.map
                    (fun n -> Literal (fixup_module_case bs.bs_path n))
                    (lib.lib_modules @ lib.lib_internal_modules)
                  @
                    [gen_getvar "EXTRA_MODULES"] ));
      Set_array(true, "OCAML_LIBS",
                ( List.map
                    (fun n -> Literal n)
                    (StrSet.elements lib_deps)
                  @
                    [gen_getvar "EXTRA_OCAML_LIBS"] ));
      Set_array(false, "C_SOURCES",
                ( List.map
                    (fun n -> Literal (get_c_object n))
                    (List.filter
                       (fun n -> OASISUnixPath.check_extension n "c")
                       bs.bs_c_sources
                    )
               ));
      Lines
        [ "C_OBJECTS = $(replacesuffixes .c, $(EXT_OBJ), $(C_SOURCES))";
        ];
      (* TODO: OCAML_CLIBS *)
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlLibrary($(NAME), $(MODULES), $(C_OBJECTS))";
        ];
      Set_array(true, "BUILD_TARGETS",
                [Expression "$(OASIS_target_OCamlLibrary $(NAME))"]);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Set_array(true, "ACCU_OCAMLINCLUDES",
                ( List.map
                    (fun n -> Literal n)
                    (StrSet.elements ocaml_includes)
                  @
                    [gen_getvar "EXTRA_OCAMLINCLUDES"] ));
      Set_array(true, "ACCU_OCAMLPACKS",
                ( List.map
                    (fun n -> Literal n)
                    ocamlpacks
                  @
                    [gen_getvar "EXTRA_OCAMLPACKS"] ));
      Export [ "BUILD_TARGETS";
               "DEFINE_RULES";
               "ACCU_OCAMLINCLUDES";
               "ACCU_OCAMLPACKS"
             ];
      (* TODO: also set OCAML_LIB_FLAGS with the contents of
         bs_ccopt, bs_cclib, bs_dllib, bs_dllpath *)
      (* TODO: also set OCAMLCFLAGS from bs_byteopt and OCAMLOPTFLAGS from
         bs_nativeopt
       *)
    ] in

  let dir = StrMap.find bs.bs_path map in
  let dir = { dir with dir_build = Section section :: dir.dir_build } in
  let map = StrMap.add bs.bs_path dir map in
  establish_in map bs module_includes

let add_executable ctx pkg map cs bs exec =
  (* CHECK: what if bs.bs_path contains .. path elements? What if module names
     do so?
   *)
  let map = establish map (new_dir bs.bs_path) in
  let lib_includes = get_lib_includes pkg bs in
  let trans_lib_deps = get_lib_deps ~transitive:true pkg bs in
  let module_includes =
    let d = OASISUnixPath.dirname exec.exec_main_is in
    if OASISUnixPath.is_current_dir d then
      StrSet.empty
    else
      StrSet.singleton d in
  let ocaml_includes =
    StrSet.union lib_includes module_includes in
  let trans_ocamlpacks = get_ocamlpacks ~transitive:true pkg bs in
  let main_module = OASISUnixPath.chop_extension exec.exec_main_is in

  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      ( match bs.bs_compiled_object with
          | Best ->
              Nop
          | Byte ->
              Set_string(false, "NATIVE_ENABLED", Literal "false")
          | Native ->
              Set_string(false, "BYTE_ENABLED", Literal "false")
      );
      Set_string(false, "MAIN_MODULE",
                 Literal(fixup_module_case bs.bs_path main_module));
      Set_array(true, "OCAMLPACKS",
                ( List.map
                    (fun n -> Literal n)
                    trans_ocamlpacks
                  @
                    [gen_getvar "EXTRA_OCAMLPACKS"] ));
      Set_array(true, "OCAML_LIBS",
                ( List.map
                    (fun n -> Literal n)
                    (StrSet.elements trans_lib_deps)
                  @
                    [gen_getvar "EXTRA_OCAML_LIBS"] ));
      Set_array(false, "C_SOURCES",
                ( List.map
                    (fun n -> Literal (get_c_object n))
                    (List.filter
                       (fun n -> OASISUnixPath.check_extension n "c")
                       bs.bs_c_sources
                    )
               ));
      Lines
        [ "C_OBJECTS = $(replacesuffixes .c, $(EXT_OBJ), $(C_SOURCES))";
        ];
      (* TODO: OCAML_CLIBS *)
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlExecutable($(NAME), $(MAIN_MODULE), $(C_OBJECTS))";
        ];
      Set_array(true, "BUILD_TARGETS",
                [Expression "$(OASIS_target_OCamlExecutable $(NAME))"]);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Set_array(true, "ACCU_OCAMLINCLUDES",
                ( List.map
                    (fun n -> Literal n)
                    (StrSet.elements ocaml_includes)
                  @
                    [gen_getvar "EXTRA_OCAMLINCLUDES"] ));
      Set_array(true, "ACCU_OCAMLPACKS", [Expression "$(OCAMLPACKS)"]);
      Export [ "BUILD_TARGETS";
               "DEFINE_RULES";
               "ACCU_OCAMLINCLUDES";
               "ACCU_OCAMLPACKS"
             ];
      (* TODO: also set OCAML_LINK_FLAGS with the contents of
         bs_ccopt, bs_cclib, bs_dllib, bs_dllpath *)
      (* TODO: also set OCAMLCFLAGS from bs_byteopt and OCAMLOPTFLAGS from
         bs_nativeopt
       *)
    ] in

  let dir = StrMap.find bs.bs_path map in
  let dir = { dir with dir_build = Section section :: dir.dir_build } in
  let map = StrMap.add bs.bs_path dir map in
  establish_in map bs module_includes


let define_build_rules =
  Lines [ "DefineBuildRules() =";
          "    OASIS_run($(DEFINE_RULES))";
        ]

let define_build_rules_empty =
  Lines [ "DefineBuildRules() =";
        ]

let finish_definitions map =
  (* add headers and footers *)
  StrMap.map
    (fun dir ->
       let build = 
         if dir.dir_build = [] then
           [ Set_array(false, "BUILD_TARGETS", []);
             define_build_rules_empty
           ]
       else
         let header =
           [ Set_array(false, "BUILD_TARGETS", []);
             Set_array(false, "DEFINE_RULES", []);
             Set_array(false, "ACCU_OCAMLINCLUDES", []);
             Set_array(false, "ACCU_OCAMLPACKS", []);
           ] in
         let footer =
           [ define_build_rules;
             Set_string(false, "OCAMLINCLUDES",
                        Expression "$(set $(ACCU_OCAMLINCLUDES))");
             Set_string(false, "OCAMLPACKS",
                        Expression "$(set $(ACCU_OCAMLPACKS))");
           ] in
         header @ dir.dir_build @ footer in
       { dir with
         dir_build = build;
       }
    )
    map

let delete_setup() =
  let p =
    OASISUnixPath.concat
      OASISUnixPath.current_dir_name 
      "_oasis_setup.om" in
  let hp = 
    OASISHostPath.of_unix p in
  if Sys.file_exists hp then
    Sys.remove hp


let write_files map =
  delete_setup();
  write_const_file
    ~skip_existing:true
    (OASISUnixPath.concat
       OASISUnixPath.current_dir_name 
       "OMakeroot")
    OMakeData.omakeroot;
  write_const_file
    (OASISUnixPath.concat
       OASISUnixPath.current_dir_name 
       "_oasis_lib.om")
    OMakeData.oasis_lib_om;
  StrMap.iter
    (fun _ dir ->
       let omf_path =
         (OASISUnixPath.concat
            dir.dir_path
            "OMakefile") in
       let omf =
         if dir.dir_top then
           OMakeData.omakefile_top
        else
           OMakeData.omakefile_nontop in
       write_const_file ~skip_existing:true omf_path omf;
       
       let hier_path =
         (OASISUnixPath.concat
            dir.dir_path
            "_oasis_hier.om") in
       let hier =
         [ Set_array(false, "OASIS_SUBDIRS",
                     List.map
                       (fun s ->
                          Literal
                            (OASISUnixPath.make_relative
                               dir.dir_path
                               s
                            )
                         )
                       dir.dir_sub)
         ] in
       write_omake_file hier_path hier;

       let build_path =
         (OASISUnixPath.concat
            dir.dir_path
            "_oasis_build.om") in
       write_omake_file build_path dir.dir_build
    )
    map


let equip_project ctxt pkg =
  let map =
    List.fold_left
      (fun acc ->
         function
         | Library (cs, bs, lib) ->
             add_library ctxt pkg acc cs bs lib
         | Executable (cs, bs, exec) ->
             add_executable ctxt pkg acc cs bs exec
         | Object (cs, bs, lib) ->
             establish acc (new_dir bs.bs_path)
         | _ ->
             acc
      )
      (new_dir_map())
      pkg.sections in
  let map =
    finish_definitions map in
  write_files map

