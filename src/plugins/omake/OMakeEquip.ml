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

    - library with -pack
    - "omake install"
    - support for objects
    - support for documents
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
      dir_install : om_entry list;
    }

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

type dir_map = dir StrMap.t

let new_dir path =
  { dir_path = path;
    dir_top = false;
    dir_sub = [];
    dir_build = [];
    dir_install = [];
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


let rec om_cond_of_expr =
  function
  | OASISExpr.EBool b ->
      OMBool b
  | OASISExpr.ENot c1 ->
      OMNot(om_cond_of_expr c1)
  | OASISExpr.EAnd(c1,c2) ->
      OMAnd(om_cond_of_expr c1, om_cond_of_expr c2)
  | OASISExpr.EOr(c1,c2) ->
      OMOr(om_cond_of_expr c1, om_cond_of_expr c2)
  | OASISExpr.EFlag name ->
      OMIsTrue(Variable ("oasis_" ^ name))
  | OASISExpr.ETest(name, value) ->
      OMEq(Variable ("oasis_" ^ OASISExpr.string_of_test name), Literal value)


let om_cond_of_flag flag =
  let rec translate =
    function
    | (e, true) :: rest -> 
        OMOr(om_cond_of_expr e, translate rest)
    | (e, false) :: rest ->
        OMAnd(OMNot(om_cond_of_expr e), translate rest)
    | [] ->
        OMBool false in
  match flag with
    | [ OASISExpr.EBool true, b ] -> OMBool b
    | [ OASISExpr.EBool false, _ ] -> OMBool false
    | _ -> translate flag


let set_array_cond append name args_choices =
  Cond(List.map
         (fun (e,args) ->
            (om_cond_of_expr e,
             [ Set_array(append, name, List.map (fun s -> Literal s) args);
               Export [name]
             ])
         )
         args_choices,
       [])
  

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


let well_known_syntax = [
  "camlp4.quotations.o";
  "camlp4.quotations.r";
  "camlp4.exceptiontracer";
  "camlp4.extend";
  "camlp4.foldgenerator";
  "camlp4.listcomprehension";
  "camlp4.locationstripper";
  "camlp4.macro";
  "camlp4.mapgenerator";
  "camlp4.metagenerator";
  "camlp4.profiler";
  "camlp4.tracer"
]


let have_syntax_camlp4o ocamlpacks =
  List.exists
    (fun pack ->
       Filename.check_suffix pack "syntax" ||
         List.mem pack well_known_syntax
    )
    ocamlpacks


let skippable name l =
  let skip = "SKIP_" ^ name in
  [ Lines
      [ "if $(not $(defined " ^ skip ^ "))";
        "    " ^ skip ^ " = false";
        "    export " ^ skip;
      ];
    Cond( [ OMNot(OMIsTrue(Variable skip)), l ],
          []);
    Export [];
  ]

  

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
      Set_array(false, "OCAML_LIBS",
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
          "C_OBJECTS += $(OASIS_getvar EXTRA_C_OBJECTS)";
        ];
      set_array_cond false "OCAML_LIB_CCLIB" bs.bs_cclib;
      Set_array(true, "OCAML_LIB_CCLIB", [gen_getvar "EXTRA_OCAML_LIB_CCLIB"]);
      set_array_cond false "OCAML_LIB_DLLIB" bs.bs_dlllib;
      Set_array(true, "OCAML_LIB_DLLIB", [gen_getvar "EXTRA_OCAML_LIB_DLLIB"]);
      set_array_cond false "OCAML_LIB_DLLPATH" bs.bs_dllpath;
      Set_array(true, "OCAML_LIB_DLLPATH",
                [gen_getvar "EXTRA_OCAML_LIB_DLLPATH"]);
      Set_array(false, "OCAML_LIB_FLAGS", [gen_getvar "EXTRA_OCAML_LIB_FLAGS"]);
      Set_array(false, "OCAMLFINDFLAGS",
                [ gen_getvar "EXTRA_OCAMLFINDFLAGS" ]);
      set_array_cond false "cflags" bs.bs_ccopt;
      set_array_cond false "ocamlcflags" bs.bs_byteopt;
      set_array_cond false "ocamloptflags" bs.bs_nativeopt;
      Lines
        [ "OCAMLCFLAGS += $(ocamlcflags)";
          "OCAMLOPTFLAGS += $(ocamloptflags)";
        ];
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlLibrary($(NAME), $(MODULES), $(C_OBJECTS))";
        ];
      Cond([ om_cond_of_flag bs.bs_build,
             [ Set_array(true, "BUILD_TARGETS",
                         [Expression "$(OASIS_target_OCamlLibrary $(NAME))"]);
               Export ["BUILD_TARGETS"];
             ]
           ],
           []);
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
      if have_syntax_camlp4o ocamlpacks then
        Set_string(false, "ACCU_SYNTAX_CAMLP4O", Literal "true")
      else
        Nop;
      Set_array(true, "ACCU_CFLAGS", [Variable "cflags"]);
      Set_array(true, "ACCU_OCAMLCFLAGS", [Variable "ocamlcflags"]);
      Set_array(true, "ACCU_OCAMLOPTFLAGS", [Variable "ocamloptflags"]);
      Export [ "BUILD_TARGETS";
               "DEFINE_RULES";
               "ACCU_OCAMLINCLUDES";
               "ACCU_OCAMLPACKS";
               "ACCU_OCAMLCFLAGS";
               "ACCU_OCAMLOPTFLAGS";
               "ACCU_CFLAGS";
               "ACCU_SYNTAX_CAMLP4O";
             ];
    ] in
  let section =
    skippable cs.cs_name section in
  let dir = StrMap.find bs.bs_path map in
  let dir = { dir with dir_build = Section section :: dir.dir_build } in
  let map = StrMap.add bs.bs_path dir map in
  establish_in map bs module_includes


let inst_library ctx pkg map cs bs lib =
  let module_files =
    List.flatten
      (List.map
         (fun m0 ->
            let m = fixup_module_case bs.bs_path m0 in
            [ Literal (m ^ ".cmi");
            ]
         )
         lib.lib_modules
      ) in
  let opt_module_files =
    List.flatten
      (List.map
         (fun m0 ->
            let m = fixup_module_case bs.bs_path m0 in
            [ Literal (m ^ ".mli");
              Literal (m ^ ".cmt");
              Literal (m ^ ".cmti");
              Literal (m ^ ".cmx");
            ]
         )
         lib.lib_modules
      ) in
  let opt_lib_files =
    [ Literal (cs.cs_name ^ ".cma");
      Literal (cs.cs_name ^ ".cmxa");
      Concat [ Literal cs.cs_name; Variable "EXT_LIB" ];
      Literal (cs.cs_name ^ ".cmxs");
      Concat [ Literal ("lib" ^ cs.cs_name ^ "_stubs"); Variable "EXT_LIB" ];
      Concat [ Literal ("dll" ^ cs.cs_name ^ "_stubs"); Variable "EXT_DLL" ];
    ] in

  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      Set_array(false, "INSTALL_FILES", 
                [ Literal "META" ] @ module_files @
                  [ gen_getvar "EXTRA_INSTALL_FILES" ]);
      Set_array(false, "INSTALL_OPTIONAL_FILES", 
                opt_module_files @ opt_lib_files @
                  [ gen_getvar "EXTRA_INSTALL_OPTIONAL_FILES" ]);
      Lines
        [ "DefineRules() =";
          "    OASIS_install_OCamlLibrary($(NAME), $(INSTALL_FILES), $(INSTALL_OPTIONAL_FILES))";
          "    OASIS_uninstall_OCamlLibrary($(NAME))";
          "    OASIS_reinstall_OCamlLibrary($(NAME), $(INSTALL_FILES), $(INSTALL_OPTIONAL_FILES))";
        ];
      Cond([ om_cond_of_flag bs.bs_install,
             [ Set_array(true, "INSTALL_TARGETS",
                         [Expression
                            "$(OASIS_installtarget_OCamlLibrary $(NAME))"]);
               Set_array(true, "UNINSTALL_TARGETS",
                         [Expression
                            "$(OASIS_uninstalltarget_OCamlLibrary $(NAME))"]);
               Set_array(true, "REINSTALL_TARGETS",
                         [Expression
                            "$(OASIS_reinstalltarget_OCamlLibrary $(NAME))"]);
               Export [ "INSTALL_TARGETS";
                        "UNINSTALL_TARGETS";
                        "REINSTALL_TARGETS";
                      ];
             ]
           ],
           []);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Export [ "INSTALL_TARGETS";
               "UNINSTALL_TARGETS";
               "REINSTALL_TARGETS";
               "DEFINE_RULES";
             ];
    ] in
  let section =
    skippable cs.cs_name section in
  let dir = StrMap.find bs.bs_path map in
  let dir = { dir with dir_install = Section section :: dir.dir_install } in
  StrMap.add bs.bs_path dir map


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
  let ocamlpacks = get_ocamlpacks ~transitive:false pkg bs in
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
      Set_array(false, "OCAMLPACKS",
                ( List.map
                    (fun n -> Literal n)
                    trans_ocamlpacks
                  @
                    [gen_getvar "EXTRA_OCAMLPACKS"] ));
      Set_array(false, "OCAML_LIBS",
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
          "C_OBJECTS += $(OASIS_getvar EXTRA_C_OBJECTS)";
        ];
      set_array_cond false "OCAML_LINK_CCLIB" bs.bs_cclib;
      Set_array(true, "OCAML_LINK_CCLIB",
                [gen_getvar "EXTRA_OCAML_LINK_CCLIB"]);
      set_array_cond false "OCAML_LINK_DLLIB" bs.bs_dlllib;
      Set_array(true, "OCAML_LINK_DLLIB",
                [gen_getvar "EXTRA_OCAML_LINK_DLLIB"]);
      set_array_cond false "OCAML_LINK_DLLPATH" bs.bs_dllpath;
      Set_array(true, "OCAML_LINK_DLLPATH",
                [gen_getvar "EXTRA_OCAML_LINK_DLLPATH"]);
      Set_array(false, "OCAML_LINK_FLAGS", 
                [ Literal "-linkpkg";
                  gen_getvar "EXTRA_OCAML_LINK_FLAGS" ]);
      Set_array(false, "OCAMLFINDFLAGS",
                [ gen_getvar "EXTRA_OCAMLFINDFLAGS" ]);
      set_array_cond false "cflags" bs.bs_ccopt;
      set_array_cond false "ocamlcflags" bs.bs_byteopt;
      set_array_cond false "ocamloptflags" bs.bs_nativeopt;
      Lines
        [ "OCAMLCFLAGS += $(ocamlcflags)";
          "OCAMLOPTFLAGS += $(ocamloptflags)";
        ];
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlExecutable($(NAME), $(MAIN_MODULE), $(C_OBJECTS))";
        ];
      Cond([ om_cond_of_flag bs.bs_build,
             [ Set_array(true, "BUILD_TARGETS",
                         [Expression "$(OASIS_target_OCamlExecutable $(NAME))"]);
               Export ["BUILD_TARGETS"];
             ]
           ],
           []);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Set_array(true, "ACCU_OCAMLINCLUDES",
                ( List.map
                    (fun n -> Literal n)
                    (StrSet.elements ocaml_includes)
                  @
                    [gen_getvar "EXTRA_OCAMLINCLUDES"] ));
      Set_array(true, "ACCU_OCAMLPACKS", [Variable "OCAMLPACKS"]);
      if have_syntax_camlp4o ocamlpacks then
        Set_string(false, "ACCU_SYNTAX_CAMLP4O", Literal "true")
      else
        Nop;
      Set_array(true, "ACCU_CFLAGS", [Variable "cflags"]);
      Set_array(true, "ACCU_OCAMLCFLAGS", [Variable "ocamlcflags"]);
      Set_array(true, "ACCU_OCAMLOPTFLAGS", [Variable "ocamloptflags"]);
      Set_array(true, "OASIS_clean_list",
                [ Expression "$(NAME)";
                  Expression "$(NAME).run";
                  Expression "$(NAME).opt";
                ]);
      Export [ "BUILD_TARGETS";
               "DEFINE_RULES";
               "ACCU_OCAMLINCLUDES";
               "ACCU_OCAMLPACKS";
               "ACCU_OCAMLCFLAGS";
               "ACCU_OCAMLOPTFLAGS";
               "ACCU_CFLAGS";
               "ACCU_SYNTAX_CAMLP4O";
               "OASIS_clean_list";
             ];
    ] in
  let section =
    skippable cs.cs_name section in

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

let define_install_rules =
  Lines [ "DefineInstallRules() =";
          "    OASIS_run($(DEFINE_RULES))";
        ]

let define_install_rules_empty =
  Lines [ "DefineInstallRules() =";
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
             Set_array(false, "ACCU_CFLAGS", []);
             Set_array(false, "ACCU_OCAMLCFLAGS", []);
             Set_array(false, "ACCU_OCAMLOPTFLAGS", []);
             Set_string(false, "ACCU_SYNTAX_CAMLP4O", Literal "false");
           ] in
         let footer =
           [ define_build_rules;
             Set_string(false, "OCAMLINCLUDES",
                        Expression "$(set $(ACCU_OCAMLINCLUDES))");
             Set_string(false, "OCAMLPACKS",
                        Expression "$(set $(ACCU_OCAMLPACKS))");
             Set_string(true, "CFLAGS",
                        Expression "$(ACCU_CFLAGS)");
             Set_string(true, "OCAMLCFLAGS",
                        Expression "$(ACCU_OCAMLCFLAGS)");
             Set_string(true, "OCAMLOPTFLAGS",
                        Expression "$(ACCU_OCAMLOPTFLAGS)");
             Lines
               [ "if $(ACCU_SYNTAX_CAMLP4O)";
                 "    OCAMLFINDFLAGS += -syntax camlp4o";
                 "    export OCAMLFINDFLAGS";
               ]
           ] in
         header @ dir.dir_build @ footer in
       let install = 
         if dir.dir_install = [] then
           [ Set_array(false, "INSTALL_TARGETS", []);
             Set_array(false, "UNINSTALL_TARGETS", []);
             Set_array(false, "REINSTALL_TARGETS", []);
             define_install_rules_empty
           ]
       else
         let header =
           [ Set_array(false, "INSTALL_TARGETS", []);
             Set_array(false, "UNINSTALL_TARGETS", []);
             Set_array(false, "REINSTALL_TARGETS", []);
             Set_array(false, "DEFINE_RULES", []);
           ] in
         let footer =
           [ define_install_rules;
           ] in
         header @ dir.dir_install @ footer in
       { dir with
         dir_build = build;
         dir_install = install;
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
       let install_path =
         (OASISUnixPath.concat
            dir.dir_path
            "_oasis_install.om") in
       write_omake_file build_path dir.dir_build;
       write_omake_file install_path dir.dir_install;
    )
    map


let equip_project ctxt pkg =
  let map =
    List.fold_left
      (fun acc ->
         function
         | Library (cs, bs, lib) ->
             let acc1 = add_library ctxt pkg acc cs bs lib in
             inst_library ctxt pkg acc1 cs bs lib
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

