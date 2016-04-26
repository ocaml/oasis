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


(** Generate standard text files: INSTALL.txt, README.txt
    @author Sylvain Le Gall
*)


open OASISFileTemplate
open OASISPlugin
open OASISTypes
open OASISUtils
open OASISValues
open OASISGettext
open OASISVersion
open OASISPlugin
open OASISSchema
open Format
open FormatExt


let plugin =
  `Extra, "StdFiles", Some OASISConf.version_short


let self_id, all_id =
  Extra.create plugin


let markdown =
  OASISFeatures.create "stdfiles_markdown" ~plugin
    OASISFeatures.alpha
    (fun () ->
       s_ "Use markdown comment and replace .txt extensions of standard files by
           .md.")

type package =
  (* Standalone executable *)
  | LTool of prog
  (* Findlib package *)
  | LFindlibPackage of name * (OASISVersion.comparator option)


let facts =
  [
    LFindlibPackage ("ocaml", None),
    [
      (* Tools shipped with OCaml *)
      "camlp4";
      "camlp4boot";
      "camlp4o";
      "camlp4of";
      "camlp4of.opt";
      "camlp4oof";
      "camlp4oof.opt";
      "camlp4o.opt";
      "camlp4orf";
      "camlp4orf.opt";
      "camlp4prof";
      "camlp4r";
      "camlp4rf";
      "camlp4rf.opt";
      "camlp4r.opt";
      "mkcamlp4";
      "ocaml";
      "ocamlbuild";
      "ocamlbuild.byte";
      "ocamlbuild.native";
      "ocamlc";
      "ocamlc.opt";
      "ocamlcp";
      "ocamldebug";
      "ocamldep";
      "ocamldep.opt";
      "ocamldoc";
      "ocamldoc.opt";
      "ocamllex";
      "ocamllex.opt";
      "ocamlmklib";
      "ocamlmktop";
      "ocamlopt";
      "ocamlopt.opt";
      "ocamlprof";
      "ocamlrun";
      "ocamlyacc";
    ],
    [
      (* Libraries shipped with OCaml *)
      "bigarray";
      "camlp4";
      "dbm";
      "dynlink";
      "graphics";
      "labltk";
      "num";
      "stdlib";
      "str";
      "threads";
      "unix";
    ];

    LFindlibPackage ("findlib", None),
    ["ocamlfind"],
    ["findlib"];
  ]


(** Merge 2 versions constraint *)
let merge_version_opt ver_opt1 ver_opt2 =
  match ver_opt1, ver_opt2 with
    | Some v1, Some v2 -> Some (VAnd (v1, v2))
    | None, v | v, None -> v


(** Associate a tool to a findlib package or a tool *)
let package_of_tool =
  let mp =
    MapString.of_list
      (List.fold_left
         (fun acc (pkg, tools, _) ->
            List.fold_left
              (fun acc tool -> (tool, pkg) :: acc)
              acc
              tools)
         [] facts)
  in
  fun tool ->
    try
      MapString.find tool mp
    with Not_found ->
      LTool tool


(** Associate a library to a findlib package raw data *)
let package_of_library =
  let mp =
    MapString.of_list
      (List.fold_left
         (fun acc (pkg, _, libs) ->
            List.fold_left
              (fun acc lib -> (lib, pkg) :: acc)
              acc
              libs)
         [] facts)
  in
  fun lib ver_opt ->
    try
      MapString.find lib mp
    with Not_found ->
      LFindlibPackage (lib, ver_opt)


type t =
  {
    readme:  unix_filename option;
    install: unix_filename option;
    authors: unix_filename option;
  }


let pivot_data =
  data_new_property plugin


let default_filenames =
  let mp =
    MapString.of_list(["README", "README.txt";
                       "INSTALL", "INSTALL.txt";
                       "AUTHORS", "AUTHORS.txt"])
  in
  fun fn ->
    try
      MapString.find fn mp
    with Not_found ->
      failwithf (f_ "No default filename for file %s.") fn

let generator =
  let fn_enable fn sync =
    let enable =
      new_field
        OASISPackage.schema
        all_id
        fn
        ~default:true
        boolean
        (fun () ->
           Printf.sprintf (f_ "Enable %s file generation.") fn)
        pivot_data
        (fun t t' -> (sync t t') <> None)
    in
    let fn =
      new_field
        OASISPackage.schema
        all_id
        (fn^"Filename")
        ~default:(default_filenames fn)
        string_not_empty
        (fun () ->
           Printf.sprintf (f_ "Real filename to use for file %s.") fn)
        pivot_data
        (fun t t' ->
           match sync t t' with
             | Some fn -> fn
             | None -> raise Not_printable)
    in
    fun data ->
      if enable data then
        Some (fn data)
      else
        None
  in

  let readme =
    fn_enable "README" (fun _ t -> t.readme)
  in

  let install =
    fn_enable "INSTALL" (fun _ t -> t.install)
  in

  let authors =
    fn_enable "AUTHORS" (fun _ t -> t.authors)
  in

  fun data ->
    {
      readme  = readme data;
      install = install data;
      authors = authors data;
    }


module SetSection =
  Set.Make
    (struct type t = section let compare = compare end)


let main ctxt pkg =
  let data = pkg.schema_data in

  (** All sections that contains a build_section *)
  let all_build_sections_set =
    let all_build_sections =
      List.rev
        (List.fold_left
           (fun acc ->
              function
                | Library (_, bs, _)
                | Object (_, bs, _)
                | Executable (_, bs, _) as sct ->
                  (sct, bs) :: acc
                | SrcRepo _ | Flag _ | Test _ | Doc _ ->
                  acc)
           []
           pkg.sections)
    in
    List.fold_left
      (fun acc e -> SetSection.add e acc)
      SetSection.empty
      (List.rev_map fst all_build_sections)
  in

  let pp_print_sections =
    (* If a tool/library applies to all sections that can handle a build_depends
       or build_tools we can say it is a general depends. In this case, return
       is None, otherwise "Some sections".
    *)
    fun fmt sections ->
      let is_all_build_sections, sections =
        if SetSection.subset all_build_sections_set sections then
          true, SetSection.diff sections all_build_sections_set
        else
          false, sections
      in
      if SetSection.is_empty sections then
        ()
      else
        fprintf fmt " for %t%a"
          (fun fmt ->
             if is_all_build_sections then
               fprintf fmt "all,@ ")
          (pp_print_list
             (fun fmt sct ->
                fprintf fmt "%s" (OASISSection.string_of_section sct))
             ",@ ")
          (SetSection.elements sections)
  in

  let pp_print_ver_opt fmt =
    may
      (fun ver_cmp ->
         fprintf fmt " (%a)"
           pp_print_string
           (OASISVersion.string_of_comparator
              (OASISVersion.comparator_reduce ver_cmp)))
  in

  let depends =
    let merge_package_section lst (new_pkg, new_sections) =
      try
        let old_pkg, new_pkg, old_sections =
          match new_pkg with
            | LTool _ as tool ->
              tool,
              tool,
              List.assoc tool lst

            | LFindlibPackage (nm1, ver_opt1) ->
              begin
                let res_opt =
                  List.fold_left
                    (fun acc pkg ->
                       match acc, pkg with
                         | None,
                           ((LFindlibPackage (nm2, ver_opt2))
                            as old_pkg,
                            sections) ->
                           if nm1 = nm2 then
                             Some
                               (old_pkg,
                                LFindlibPackage
                                  (nm1,
                                   merge_version_opt
                                     ver_opt1
                                     ver_opt2),
                                sections)
                           else
                             acc
                         | _, _ ->
                           acc)
                    None
                    lst
                in
                match res_opt with
                  | Some res ->
                    res
                  | None ->
                    raise Not_found
              end
        in
        let lst =
          try
            List.remove_assoc old_pkg lst
          with Not_found ->
            lst
        in
        (new_pkg, SetSection.union new_sections old_sections) :: lst

      with Not_found ->
        (new_pkg, new_sections) :: lst
    in

    let add_build_tools ssection =
      List.fold_left
        (fun lst ->
           function
             | ExternalTool tool ->
               (package_of_tool tool, ssection) :: lst
             | InternalExecutable _ ->
               lst)
    in

    let split_package_section =
      List.fold_left
        (fun lst ->
           function
             | Library (_, bs, _)
             | Object (_, bs, _)
             | Executable (_, bs, _) as section ->
               begin
                 let ssection =
                   SetSection.singleton section
                 in
                 let lst =
                   (* Add build_depends *)
                   List.fold_left
                     (fun lst ->
                        function
                          | FindlibPackage (fndlb_nm, ver_opt) ->
                            let fndlb_root =
                              match (OASISString.nsplit fndlb_nm '.') with
                                | hd :: _ -> hd
                                | _ -> fndlb_nm
                            in
                            (package_of_library fndlb_root ver_opt,
                             ssection) :: lst
                          | InternalLibrary _ ->
                            lst)
                     lst
                     bs.bs_build_depends
                 in
                 (* Add build_tools *)
                 add_build_tools
                   ssection
                   lst
                   bs.bs_build_tools
               end
             | Test (_, {test_tools = build_tools})
             | Doc (_, {doc_build_tools = build_tools}) as section ->
               add_build_tools
                 (SetSection.singleton section)
                 lst
                 build_tools
             | Flag _ | SrcRepo _ ->
               lst)

        (* Basic dependencies *)
        ([
          LFindlibPackage ("findlib", pkg.findlib_version),
          all_build_sections_set;

          LFindlibPackage ("ocaml", pkg.ocaml_version),
          all_build_sections_set]
         @
           (if OASISFeatures.package_test
               OASISFeatures.dynrun_for_release pkg then
              [
                LFindlibPackage ("oasis",
                  Some (OASISVersion.VGreaterEqual
                      pkg.oasis_version)),
                all_build_sections_set
              ]
            else
              []))


        (* Go through all sections *)
        pkg.sections
    in

    (* Merge everything *)
    List.fold_left
      merge_package_section
      []
      split_package_section
  in

  let add_file ctxt (fn_opt, important, ppf) =
    match fn_opt with
      | Some unix_fn ->
        begin
          let rec remove_eol_eof str =
            List.fold_left
              (fun str eol ->
                 try
                   remove_eol_eof (OASISString.strip_ends_with ~what:eol str)
                 with Not_found ->
                   str)
              str ["\n"; "\r"]
          in
          let content =
            ppf str_formatter;
            (* Trim problematic 2x newline at end of last para, because we
             * don't know if there will a following para.
            *)
            remove_eol_eof (flush_str_formatter ())
          in
          let comment =
            if OASISFeatures.package_test markdown pkg then
              comment_markdown
            else
              comment_ml
          in
          add_file
            {(template_make unix_fn comment
                []
                [
                  (* One line before, one line after to avoid mixing content
                   * with comment formatting (it causes e.g. pandoc to think
                   * this is a paragraph.
                  *)
                  "\n"^content^"\n"
                ]
                []) with
               important = important}
            ctxt
        end

      | None ->
        ctxt
  in

  let t = generator data in

  let fix_ext fn real_fn_opt =
    match real_fn_opt with
      | Some real_fn ->
        if OASISFeatures.package_test markdown pkg &&
           real_fn = default_filenames fn then
          Some (fn^".md")
        else
          Some real_fn
      | None -> None
  in

  let t =
    {
      readme = fix_ext "README" t.readme;
      install = fix_ext "INSTALL" t.install;
      authors = fix_ext "AUTHORS" t.authors;
    }
  in

  List.fold_left
    add_file
    ctxt
    [
      (* Generate README.txt *)
      t.readme,
      false,
      (fun fmt ->
         pp_open_vbox fmt 0;
         pp_print_titlef fmt 1 "%s - %s" pkg.name pkg.synopsis;

         may
           (fun descr ->
              OASISText.pp_print fmt descr;
              pp_print_newline fmt ();
              pp_print_newline fmt ())
           pkg.description;

         may
           (fun fn ->
              pp_print_paraf fmt
                "See the file [%s](%s) for building and installation \
                 instructions." fn fn)
           t.install;

         may (pp_print_paraf fmt "[Home page](%s)") pkg.homepage;

         pp_print_title fmt 2 "Copyright and license";

         List.iter
           (fun str ->
              pp_print_string fmt str;
              pp_print_newline fmt ())
           pkg.copyrights;
         if pkg.copyrights <> [] then
           pp_print_newline fmt ();

         pp_print_para fmt
           (OASISLicense.legal_disclaimer pkg.name pkg.license);

         may
           (fun fn ->
              pp_print_paraf fmt "See [%s](%s) for more information." fn fn)
           pkg.license_file;

         pp_close_box fmt ();
      );

      (* Generate INSTALL.txt *)
      t.install,
      (OASISFeatures.package_test OASISFeatures.dynrun_for_release pkg),
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt
           "@[This is the INSTALL file for the %s distribution.@]@,@,"
           pkg.name;

         pp_print_para fmt
           "This package uses OASIS to generate its build system. \
            See section OASIS for full information.";

         pp_print_title fmt 1 "Dependencies";
         fprintf fmt "@[In order to compile this package, you will need:@]@,";
         pp_open_vbox fmt 0;
         pp_print_cut fmt ();
         pp_print_list
           (fun fmt (pkg, sections) ->
              fprintf fmt "* @[%a%a@]"
                (fun fmt ->
                   function
                     | LTool s ->
                       pp_print_string fmt s
                     | LFindlibPackage (nm, ver_opt) ->
                       fprintf fmt "%s%a"
                         nm
                         pp_print_ver_opt ver_opt)
                pkg
                pp_print_sections sections)
           "@,"
           fmt
           depends;

         pp_close_box fmt ();
         pp_print_cut fmt ();

         pp_print_string fmt StdFilesData.install;
         pp_close_box fmt ());

      (* Generate AUTHORS.txt *)
      t.authors,
      false,
      (fun fmt ->
         pp_open_vbox fmt 0;
         fprintf fmt "@[Authors of %s:@]@," pkg.name;
         pp_print_cut fmt ();
         pp_print_list
           (fun fmt str -> pp_print_string fmt ("* "^str))
           "@,"
           fmt
           pkg.OASISTypes.authors;

         if pkg.maintainers <> [] then begin
           pp_print_cut2 fmt ();
           fprintf fmt "@[Current maintainers of %s:@]@," pkg.name;
           pp_print_cut fmt ();
           pp_print_list
             (fun fmt str -> pp_print_string fmt ("* "^str))
             "@,"
             fmt
             pkg.maintainers
         end;

         pp_close_box fmt ());
    ]


let init () =
  register_help plugin
    {(help_default StdFilesData.readme_template_mkd) with
       help_order = 50};
  Extra.register_act self_id main;
  register_generator_package all_id pivot_data generator

