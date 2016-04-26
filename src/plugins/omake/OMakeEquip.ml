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

(* TODO: support for objects *)

open OASISPlugin
open OASISTypes
open OMakeFields
open OMakeFormat
open Printf

type dir =
  { dir_path : string;
    dir_top : bool;
    dir_sub : string list;
    dir_build : om_entry list;
    dir_install : om_entry list;
    dir_pack : bool;
  }

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

type dir_map = dir StrMap.t

let fixup_path path =
  if path = "/" then (* CHECK: can we really run into this? *)
    failwith "Absolute paths not supported here";
  let l = OASISString.nsplit path '/' in
  let l = List.filter (fun n -> n <> "") l in
  String.concat "/" l

let new_dir path =
  { dir_path = fixup_path path;
    dir_top = false;
    dir_sub = [];
    dir_build = [];
    dir_install = [];
    dir_pack = false;
  }

let new_dir_map() =
  let top_dir = new_dir OASISUnixPath.current_dir_name in
  let top_dir = { top_dir with dir_top = true } in
  StrMap.add top_dir.dir_path top_dir StrMap.empty

let rec establish map dir =
  if StrMap.mem dir.dir_path map then
    map
  else
    let container = OASISUnixPath.dirname dir.dir_path in
    let cont_dir = new_dir container in
    let map1 = establish map cont_dir in
    let cont_dir1 = StrMap.find cont_dir.dir_path map1 in
    let cont_dir2 =
      { cont_dir1 with dir_sub = dir.dir_path :: cont_dir1.dir_sub } in
    let map2 = StrMap.add cont_dir.dir_path cont_dir2 map1 in
    StrMap.add dir.dir_path dir map2


let establish_in map path module_includes =
  (* Also create OMakefile in all directories storing the module files: *)
  StrSet.fold
    (fun include_dir acc ->
       establish map (new_dir (OASISUnixPath.concat path include_dir))
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
      (List.rev args_choices),
    [])


let gen_getvar name =
  Expression (sprintf "$(OASIS_getvar %s)" name)


exception Ident of string


let get_data_destination ?(default_dest = "$datadir/$pkg_name") dir_opt =
  (* cannot use Buffer.add_substitute because we want to get an omake
     expression as result *)
  let dir =
    match dir_opt with
      | None -> default_dest
      | Some dir -> dir in
  let lim = String.length dir in
  let get_lit acc =
    if acc = [] then
      []
    else
      [Literal (String.concat "" (List.rev acc))] in
  let rec subst acc prev i =
    if i < lim then (
      match dir.[i] with
        | '$' when prev = '\\' ->
          subst ("$"::acc) ' ' (i+1)
        | '$' ->
          (* a gross hack... *)
          ( try
              let b = Buffer.create 1 in
              Buffer.add_substitute
                b
                (fun ident -> raise(Ident ident))
                (String.sub dir i (lim-i));
              assert false
            with Ident ident ->
              let len =
                if i+1 < lim && (dir.[i+1] = '(' || dir.[i+1] = '{') then
                  String.length ident + 3
                else
                  String.length ident + 1 in
              get_lit acc @
                [Variable ("oasis_" ^ ident)] @
                subst [] ' ' (i+len)
          )
        | cur when prev = '\\' ->
          let acc = ("\\" ^ String.make 1 cur) :: acc in
          subst acc ' ' (i+1)
        | '\\' ->
          subst acc '\\' (i+1)
        | cur ->
          subst (String.make 1 cur :: acc) cur (i+1)
    ) else
      let acc =
        if prev = '\\' then String.make 1 prev :: acc else acc in
      get_lit acc in
  subst [] ' ' 0


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


let get_lib_flname cs lib =
  String.concat
    "."
    ( (match lib.lib_findlib_parent with
        | None -> []
        | Some p -> [p]
      ) @ lib.lib_findlib_containers @
        match lib.lib_findlib_name with
          | None -> [cs.cs_name]
          | Some n -> [n]
    )


let rebase_lib_dir pkg cur_path lib_sect_name =
  let sect2 =
    try OASISSection.section_find
          (`Library,lib_sect_name) pkg.sections
    with Not_found ->
      failwith
        (sprintf "Cannot find section: %s" lib_sect_name) in
  ( match sect2 with
    | Library(_,sect_bs,_) ->
      OASISUnixPath.make_relative
        cur_path
        sect_bs.bs_path
    | _ ->
      assert false
  )


let rebase_lib pkg cur_path lib_sect_name =
  let p = rebase_lib_dir pkg cur_path lib_sect_name in
  OASISUnixPath.concat p lib_sect_name


let get_lib_includes_1 pkg cur_path libs =
  (* only the direct includes, not the indirect ones *)
  strset_flatten
    (List.map
       (fun sect_name ->
          let p = rebase_lib_dir pkg cur_path sect_name in
          StrSet.singleton p
       )
       libs
    )

let get_lib_includes pkg bs =
  let libs =
    List.flatten
      (List.map
         (function
           | FindlibPackage _ -> []
           | InternalLibrary sect_name -> [sect_name]
         )
         bs.bs_build_depends
      ) in
  get_lib_includes_1 pkg bs.bs_path libs


let get_lib_deps ?(transitive=false) ?(filter = fun _ -> true) pkg bs =
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
                    if filter sect_name2 then
                      let p = rebase_lib pkg bs.bs_path sect_name2 in
                      StrSet.singleton p
                    else
                      StrSet.empty
                )
                deps
             )
       )
       bs.bs_build_depends
    )


let lib_has_c_sources pkg sect_name =
  let sect =
    try OASISSection.section_find (`Library,sect_name) pkg.sections
    with Not_found ->
      failwith (sprintf "Cannot find section: %s" sect_name) in
  match sect with
    | Library(cs,bs,lib) ->
      bs.bs_c_sources <> []
    | _ ->
      false


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


let skippable prefix name l =
  let skip = prefix ^ "_" ^ name in
  [ Lines
      [ "if $(not $(defined " ^ skip ^ "))";
        "    " ^ skip ^ " = false";
        "    export " ^ skip;
      ];
    Cond( [ OMNot(OMIsTrue(Variable skip)), l ],
      []);
    Export [];
  ]


let set_byte_or_native bs =
  (* the default settings correspond to "best" *)
  match bs.bs_compiled_object with
    | Best ->
      Nop
    | Byte ->
      Set_string(false, "NATIVE_ENABLED", Literal "false")
    | Native ->
      Set_string(false, "BYTE_ENABLED", Literal "false")


let add_library ctx pkg map cs bs lib =
  (* CHECK: what if bs.bs_path contains .. path elements? What if module names
     do so?
  *)
  let lib_dir = new_dir bs.bs_path in
  if lib_dir.dir_pack then
    failwith ("It is not supported to build a second library in a directory \
               where already a packed library is built");
  let map = establish map lib_dir in
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
  let module_impls =
    (* TODO: at this point we'd like to filter out modules that only consist
       of a mli file. We cannot know that, though, because there may be a
       rule to generate the ml file.
    *)
    List.flatten
      (List.map
         (fun m ->
            let m = fixup_module_case bs.bs_path m in
            [ Literal m ]
         )
         (lib.lib_modules @ lib.lib_internal_modules)
      ) in
  let priv_modules = "MODULES_" ^ cs.cs_name in
  let priv_c_sources = "C_SOURCES_" ^ cs.cs_name in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      Set_string(false, "CNAME", Literal (String.capitalize cs.cs_name));
      set_byte_or_native bs;
      Set_array(false, "MODULES", module_impls @
          [gen_getvar "EXTRA_MODULES"] );
      Set_array(false, priv_modules, [ Variable "MODULES" ]);
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
      Set_array(false, priv_c_sources, [ Variable "C_SOURCES" ]);
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
      if lib.lib_pack then
        Set_array(false, "ocamloptflags_late", [ Expression "-for-pack";
                                                 Expression "$(CNAME)" ])
      else
        Set_array(false, "ocamloptflags_late", []);
      Lines
        [ "OCAMLCFLAGS += $(ocamlcflags)";
          "OCAMLOPTFLAGS += $(ocamloptflags)";
        ];
      Lines
        [ "DefineRules() =" ];
      if lib.lib_pack then
        Lines
          [ "    OASIS_build_OCamlPack($(NAME), $(MODULES))";
            "    OASIS_build_OCamlLibrary($(NAME), $(NAME), $(C_OBJECTS))"
          ]
      else
        Lines
          [ "    OASIS_build_OCamlLibrary($(NAME), $(MODULES), $(C_OBJECTS))" ];
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
      Set_array(true, "ACCU_OCAMLOPTFLAGS", [Variable "ocamloptflags_late"]);
      Export [ "BUILD_TARGETS";
               "DEFINE_RULES";
               "ACCU_OCAMLINCLUDES";
               "ACCU_OCAMLPACKS";
               "ACCU_OCAMLCFLAGS";
               "ACCU_OCAMLOPTFLAGS";
               "ACCU_CFLAGS";
               "ACCU_SYNTAX_CAMLP4O";
               priv_modules;
               priv_c_sources;
             ];
    ] in
  let section =
    skippable "SKIP_BUILD" cs.cs_name section in
  let dir = StrMap.find lib_dir.dir_path map in
  let dir = { dir with
                dir_build = Section section :: dir.dir_build;
                dir_pack = lib.lib_pack;
            } in
  let map = StrMap.add lib_dir.dir_path dir map in
  establish_in map bs.bs_path module_includes


let inst_data ?(typ="OCamlLibrary") ?default_dest data_files =
  let lines1 =
    [ sprintf "    OASIS_uninstall_data_%s($(NAME))" typ;
    ] in
  let lines2 =
    List.flatten
      (List.map
         (fun (src, dest) ->
            let dest_val = get_data_destination ?default_dest dest in
            [ sprintf "    OASIS_install_data_%s($(NAME), %s, %s)"
                typ src (string_of_value (Concat dest_val));
              sprintf "    OASIS_reinstall_data_%s($(NAME), %s, %s)"
                typ src (string_of_value (Concat dest_val));
            ]
         )
         data_files
      ) in
  Lines (lines1 @ lines2)


let inst_library ctx pkg map cs bs lib =
  let lib_dir = new_dir bs.bs_path in
  let findlib_parent_section =
    match lib.lib_findlib_parent with
      | None -> None
      | Some p ->
        ( try
            Some
              (List.find
                 (fun sect ->
                    match sect with
                      | Library(cs,bs,lib) ->
                        get_lib_flname cs lib = p
                      | _ ->
                        false
                 )
                 pkg.sections
              )
          with
            | Not_found ->
              failwith ("No section with this findlib name: " ^ p)
        ) in
  let findlib_parent_relpath =
    match findlib_parent_section with
      | None -> None
      | Some(Library(_,pbs,_)) ->
        Some(OASISUnixPath.make_relative bs.bs_path pbs.bs_path)
      | Some _ ->
        assert false in
  let modules =
    List.map
      (fun m0 ->
         let m = fixup_module_case bs.bs_path m0 in
         Literal m
      )
      lib.lib_modules in
  let maybe_meta =
    if lib.lib_findlib_parent = None then [Literal "META"] else [] in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      Set_string(false, "FINDLIB_NAME", Literal (get_lib_flname cs lib));
      Set_string(false, "FINDLIB_PARENT",
        match lib.lib_findlib_parent, findlib_parent_relpath with
          | None, None -> Variable "FINDLIB_NAME"
          | Some p, Some path -> Literal(OASISUnixPath.concat path p)
          | _ -> assert false);
      set_byte_or_native bs;
      Set_array(false, "INSTALL_MODULES",
        modules @
          [ gen_getvar "EXTRA_INSTALL_MODULES" ]);
      Set_array(false, "INSTALL_FILES",
        maybe_meta @
          [ Expression "$(OASIS_expand_module_files_OCamlLibrary $(INSTALL_MODULES))";
            Expression "$(OASIS_expand_library_files_OCamlLibrary $(NAME))";
            gen_getvar "EXTRA_INSTALL_FILES"
          ]);
      Set_array(false, "INSTALL_OPTIONAL_FILES",
        [ Expression "$(OASIS_expand_optional_module_files_OCamlLibrary $(INSTALL_MODULES))";
          Expression "$(OASIS_expand_optional_library_files_OCamlLibrary $(NAME))";
          gen_getvar "EXTRA_INSTALL_OPTIONAL_FILES"
        ]);
      Lines
        [ "DefineRules() =";
          "    OASIS_install_OCamlLibrary($(NAME), $(FINDLIB_NAME), $(FINDLIB_PARENT), $(INSTALL_FILES), $(INSTALL_OPTIONAL_FILES))";
          "    OASIS_uninstall_OCamlLibrary($(NAME), $(FINDLIB_NAME), $(FINDLIB_PARENT))";
          "    OASIS_reinstall_OCamlLibrary($(NAME), $(FINDLIB_NAME), $(FINDLIB_PARENT), $(INSTALL_FILES), $(INSTALL_OPTIONAL_FILES))";
        ];
      inst_data bs.bs_data_files;
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
    skippable "SKIP_INSTALL" cs.cs_name section in
  let dir = StrMap.find lib_dir.dir_path map in
  let dir = { dir with dir_install = Section section :: dir.dir_install } in
  StrMap.add lib_dir.dir_path dir map


let add_document ctx pkg map cs doc =
  let path = DocFields.path cs.cs_data in
  let libs_findlib = DocFields.libraries cs.cs_data in
  let intro = DocFields.intro cs.cs_data in
  let modules = DocFields.modules cs.cs_data in
  let texts = DocFields.texts cs.cs_data in

  let doc_dir = new_dir path in
  let map = establish map doc_dir in

  let _, _, library_name_of_findlib_name =
    OASISFindlib.findlib_mapping pkg in
  let libs =
    List.map library_name_of_findlib_name libs_findlib in
  let libs =
    List.map (rebase_lib pkg path) libs in

(*
  let lib_includes =
    get_lib_includes_1 pkg path libs in
 *)
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
      modules in
(*
  let ocaml_includes =
    StrSet.union lib_includes module_includes in
 *)
  let module_impls =
    List.flatten
      (List.map
         (fun m ->
            let m = fixup_module_case path m in
            [ Literal m ]
         )
         modules
      ) in
  let text_lits =
    List.map (fun s -> Literal s) texts in
  let intro_lits =
    match intro with
      | None -> []
      | Some intr -> [Literal intr] in
  let format = string_of_format doc.doc_format in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      Set_string(false, "FORMAT", Literal format);
      Set_array(false, "MODULES", module_impls @
          [gen_getvar "EXTRA_MODULES"] );
      Set_array(false, "TEXTS", text_lits @
          [gen_getvar "EXTRA_TEXTS"] );
      Set_array(false, "INTRO", intro_lits);
      Set_array(false, "OCAML_LIBS",
        ( List.map
            (fun n -> Literal n)
            libs
          @
            [gen_getvar "EXTRA_OCAML_LIBS"] ));
      Set_array(false, "OCAMLDOCFLAGS",
        [ gen_getvar "EXTRA_OCAMLDOCFLAGS" ]);
      Set_array(false, "OCAMLDOCFLAGS_HTML",
        [ gen_getvar "EXTRA_OCAMLDOCFLAGS_HTML" ]);
      Set_array(false, "OCAMLDOCFLAGS_LATEX",
        [ gen_getvar "EXTRA_OCAMLDOCFLAGS_LATEX" ]);
      Set_array(false, "OCAMLFINDFLAGS",
        [ gen_getvar "EXTRA_OCAMLFINDFLAGS" ]);
      Set_array(true, "TEXINPUTS", [Expression "$(NAME).doc/latex"]);
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlDoc($(NAME), $(MODULES), $(TEXTS))";
        ];
      Cond([ om_cond_of_flag doc.doc_build,
             [ Set_array(true, "BUILD_DOC_TARGETS",
                 [Expression "$(OASIS_target_OCamlDoc $(NAME), $(FORMAT))"]);
               Export ["BUILD_DOC_TARGETS"];
             ]
           ],
        []);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Export [ "BUILD_DOC_TARGETS";
               "DEFINE_RULES";
             ];
    ] in
  let section =
    skippable "SKIP_DOC" cs.cs_name section in
  let dir = StrMap.find doc_dir.dir_path map in
  let dir = { dir with dir_build = Section section :: dir.dir_build } in
  let map = StrMap.add doc_dir.dir_path dir map in
  establish_in map path module_includes


let inst_document ctx pkg map cs doc =
  let path = DocFields.path cs.cs_data in
  let doc_dir = new_dir path in
  let format = string_of_format doc.doc_format in
  let dest = get_data_destination (Some doc.doc_install_dir) in
  let data_files = doc.doc_data_files in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      Set_string(false, "FORMAT", Literal format);
      Set_string(false, "DEST", Concat dest);
      Set_array(false, "INSTALL_FILES",
        [ Expression
            "$(OASIS_expand_files_Document $(NAME), $(FORMAT))";
          gen_getvar "EXTRA_INSTALL_FILES"
        ]);
      Lines
        [ "DefineRules() =";
          "    OASIS_install_Document($(NAME), $(INSTALL_FILES), $(DEST))";
          "    OASIS_uninstall_Document($(NAME))";
          "    OASIS_reinstall_Document($(NAME), $(INSTALL_FILES), $(DEST))";
        ];
      inst_data ~typ:"Document" ~default_dest:doc.doc_install_dir data_files;
      Cond([ om_cond_of_flag doc.doc_install,
             [ Set_array(true, "INSTALL_TARGETS",
                 [Expression
                    "$(OASIS_installtarget_Document $(NAME))"]);
               Set_array(true, "UNINSTALL_TARGETS",
                 [Expression
                    "$(OASIS_uninstalltarget_Document $(NAME))"]);
               Set_array(true, "REINSTALL_TARGETS",
                 [Expression
                    "$(OASIS_reinstalltarget_Document $(NAME))"]);
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
    skippable "SKIP_DOC_INSTALL" cs.cs_name section in
  let dir = StrMap.find doc_dir.dir_path map in
  let dir = { dir with dir_install = Section section :: dir.dir_install } in
  StrMap.add doc_dir.dir_path dir map


let add_executable ctx pkg map cs bs exec =
  (* CHECK: what if bs.bs_path contains .. path elements? What if module names
     do so?
  *)
  let exec_dir = new_dir bs.bs_path in
  if exec_dir.dir_pack then
    failwith ("It is not supported to build an executable in a directory \
               where already a packed library is built");
  let map = establish map exec_dir in
  let lib_includes = get_lib_includes pkg bs in
  let trans_lib_deps = get_lib_deps ~transitive:true pkg bs in
  let clib_deps =
    get_lib_deps
      ~transitive:true
      ~filter:(lib_has_c_sources pkg)
      pkg bs in
  let auto_cclib =
    List.map
      (fun p -> Literal ("-L" ^ OASISUnixPath.dirname p))
      (StrSet.elements clib_deps) in
  let auto_dllpath =
    List.map
      (fun p -> Literal (OASISUnixPath.dirname p))
      (StrSet.elements clib_deps) in
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
      set_byte_or_native bs;
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
        auto_cclib @ [gen_getvar "EXTRA_OCAML_LINK_CCLIB"]);
      set_array_cond false "OCAML_LINK_DLLIB" bs.bs_dlllib;
      Set_array(true, "OCAML_LINK_DLLIB",
        [gen_getvar "EXTRA_OCAML_LINK_DLLIB"]);
      set_array_cond false "OCAML_LINK_DLLPATH" bs.bs_dllpath;
      Set_array(true, "OCAML_LINK_DLLPATH",
        auto_dllpath @ [gen_getvar "EXTRA_OCAML_LINK_DLLPATH"]);
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
    skippable "SKIP_BUILD" cs.cs_name section in

  let dir = StrMap.find exec_dir.dir_path map in
  let dir = { dir with dir_build = Section section :: dir.dir_build } in
  let map = StrMap.add exec_dir.dir_path dir map in
  establish_in map bs.bs_path module_includes


let inst_executable ctx pkg map cs bs exec =
  let exec_dir = new_dir bs.bs_path in
  let section =
    [ Set_string(false, "NAME", Literal cs.cs_name);
      set_byte_or_native bs;
      Set_string(false, "INSTALL_FILE",
        Expression "$(OASIS_expand_file_Executable $(NAME))");
      Lines
        [ "DefineRules() =";
          "    OASIS_install_Executable($(NAME), $(INSTALL_FILE))";
          "    OASIS_uninstall_Executable($(NAME))";
          "    OASIS_reinstall_Executable($(NAME), $(INSTALL_FILE))";
        ];
      inst_data ~typ:"Executable" bs.bs_data_files;
      Cond([ om_cond_of_flag bs.bs_install,
             [ Set_array(true, "INSTALL_TARGETS",
                 [Expression
                    "$(OASIS_installtarget_Executable $(NAME))"]);
               Set_array(true, "UNINSTALL_TARGETS",
                 [Expression
                    "$(OASIS_uninstalltarget_Executable $(NAME))"]);
               Set_array(true, "REINSTALL_TARGETS",
                 [Expression
                    "$(OASIS_reinstalltarget_Executable $(NAME))"]);
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
    skippable "SKIP_INSTALL" cs.cs_name section in
  let dir = StrMap.find exec_dir.dir_path map in
  let dir = { dir with dir_install = Section section :: dir.dir_install } in
  StrMap.add exec_dir.dir_path dir map


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
             Set_array(false, "BUILD_DOC_TARGETS", []);
             define_build_rules_empty;
             Lines [ "" ];
           ]
         else
           let header =
             [ Set_array(false, "BUILD_TARGETS", []);
               Set_array(false, "BUILD_DOC_TARGETS", []);
               Set_array(false, "DEFINE_RULES", []);
               (* "." is required for OCAMLDEP_MODULES_ENABLED! *)
               Set_array(false, "ACCU_OCAMLINCLUDES", [ Literal "." ]);
               Set_array(false, "ACCU_OCAMLPACKS", []);
               Set_array(false, "ACCU_CFLAGS", []);
               Set_array(false, "ACCU_OCAMLCFLAGS", []);
               Set_array(false, "ACCU_OCAMLOPTFLAGS", []);
               Set_string(false, "ACCU_SYNTAX_CAMLP4O", Literal "false");
               Lines [ "" ];
             ] in
           let footer =
             [ Lines [ "" ];
               define_build_rules;
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
                   ""
                 ]
             ] in
           header @ dir.dir_build @ footer in
       let install =
         if dir.dir_install = [] then
           [ Set_array(false, "INSTALL_TARGETS", []);
             Set_array(false, "UNINSTALL_TARGETS", []);
             Set_array(false, "REINSTALL_TARGETS", []);
             define_install_rules_empty;
             Lines [ "" ];
           ]
         else
           let header =
             [ Set_array(false, "INSTALL_TARGETS", []);
               Set_array(false, "UNINSTALL_TARGETS", []);
               Set_array(false, "REINSTALL_TARGETS", []);
               Set_array(false, "DEFINE_RULES", []);
               Lines [ "" ];
             ] in
           let footer =
             [ Lines [ "" ];
               define_install_rules;
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
  Printexc.record_backtrace true;
  try
    let map =
      List.fold_left
        (fun acc ->
           function
             | Library (cs, bs, lib) ->
               let acc1 = add_library ctxt pkg acc cs bs lib in
               inst_library ctxt pkg acc1 cs bs lib
             | Executable (cs, bs, exec) ->
               let acc1 = add_executable ctxt pkg acc cs bs exec in
               inst_executable ctxt pkg acc1 cs bs exec
             | Object (cs, bs, lib) ->
               (* TODO *)
               establish acc (new_dir bs.bs_path)
             | Doc(cs,doc) ->
               let acc1 = add_document ctxt pkg acc cs doc in
               inst_document ctxt pkg acc1 cs doc
             | _ ->
               acc
        )
        (new_dir_map())
        pkg.sections in
    let map =
      finish_definitions map in
    write_files map
  with
    | err ->
      let bt = Printexc.get_backtrace() in
      eprintf "Backtrace: %s\n%!" bt;
      raise err
