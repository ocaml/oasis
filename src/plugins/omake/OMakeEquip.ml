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
    let cont_dir1 = StrMap.find container map in
    let cont_dir2 =
      { cont_dir1 with dir_sub = dir.dir_path :: cont_dir1.dir_sub } in
    let map2 = StrMap.add container cont_dir2 map1 in
    StrMap.add dir.dir_path dir map2


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


let add_library ctx pkg map cs bs lib =
  let map = establish map (new_dir bs.bs_path) in
  let ocaml_includes =
    (* only the direct includes, not the indirect ones *)
    List.flatten
      (List.map
         (function
           | FindlibPackage _ -> []
           | InternalLibrary sect_name ->
               let sect =
                 try OASISSection.section_find (`Library,sect_name) pkg.sections
                 with Not_found ->
                   failwith (sprintf "Cannot find section: %s" sect_name) in
               ( match sect with
                   | Library(_,sect_bs,_) ->
                       [ OASISUnixPath.make_relative
                           bs.bs_path
                           sect_bs.bs_path
                       ]
                   | _ ->
                       []
               )

         )
         bs.bs_build_depends
      ) in
  let ocamlpacks =
    List.flatten
      (List.map
         (function
           | FindlibPackage(flib,_) -> [flib]
           | InternalLibrary _ -> []
         )
         bs.bs_build_depends
      ) in

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
      Set_array(true, "OCAMLINCLUDES",
                ( List.map
                    (fun n -> Literal n)
                    ocaml_includes
                  @
                    [gen_getvar "EXTRA_OCAMLINCLUDES"] ));
      Set_array(true, "OCAMLPACKS",
                ( List.map
                    (fun n -> Literal n)
                    ocamlpacks
                  @
                    [gen_getvar "EXTRA_OCAMLPACKS"] ));
      Lines
        [ "DefineRules() =";
          "    OASIS_build_OCamlLibrary($(NAME), $(MODULES))";
        ];
      Set_array(true, "BUILD_TARGETS",
                [Expression "$(OASIS_target_OCamlLibrary $(NAME))"]);
      Set_array(true, "DEFINE_RULES", [Expression "$(DefineRules)"]);
      Set_array(true, "ACCU_OCAMLINCLUDES", [Expression "$(OCAMLINCLUDES)"]);
      Set_array(true, "ACCU_OCAMLPACKS", [Expression "$(OCAMLPACKS)"]);
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
  StrMap.add bs.bs_path dir map

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

let write_files map =
  write_const_file
    ~skip_existing:true
    (OASISUnixPath.concat
       OASISUnixPath.current_dir_name 
       "OMakeroot")
    OMakeData.omakeroot;
  write_omake_file
    (OASISUnixPath.concat
       OASISUnixPath.current_dir_name 
       "_oasis_setup.om")
    [ Lines [ "section";
              "    err. =";
              "        extends $(Exception)";
              "        message = $'Not configured. Run first: ocaml setup.ml -configure'";
              "    raise $(err)"
            ]
    ];
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
                     List.map (fun s -> Literal s) dir.dir_sub)
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
         | Executable (cs, bs, lib) ->
             establish acc (new_dir bs.bs_path)
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

