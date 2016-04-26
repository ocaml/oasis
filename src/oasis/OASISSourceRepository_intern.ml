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


(** SourceRepository schema and generator (internal)
    @author Sylvain Le Gall
*)


open OASISSchema_intern
open OASISValues
open OASISUtils
open OASISGettext
open PropList.Field
open OASISTypes


let schema, generator =
  let schm =
    schema "SourceRepository" (fun (cs, _) -> cs.cs_plugin_data)
  in
  let cmn_section_gen =
    OASISSection_intern.section_fields
      (fun () -> (s_ "source repository"))
      schm
      (fun (cs, _) -> cs)
  in
  let typ =
    new_field schm "Type"
      (choices
         (fun () -> s_ "source repository type")
         ["darcs",    Darcs;
          "git",      Git;
          "svn",      Svn;
          "cvs",      Cvs;
          "hg",       Hg;
          "bzr",      Bzr;
          "arch",     Arch;
          "monotone", Monotone])
      (fun () -> s_ "VCS type.")
      (fun (_, src_repo) -> src_repo.src_repo_type)
  in
  let location =
    new_field schm "Location"
      url
      (fun () ->
         s_ "URL of the repository. The exact form of this field depends on \
             the repository type.")
      (fun (_, src_repo) -> src_repo.src_repo_location)
  in
  let browser =
    new_field schm "Browser"
      ~default:None
      (opt url)
      (fun () ->
         s_ "URL where the repository can be navigated using a web browser.")
      (fun (_, src_repo) -> src_repo.src_repo_browser)
  in
  let new_field_opt nm hlp =
    new_field schm nm
      ~default:None
      (opt string_not_empty)
      hlp
  in
  let modul =
    new_field_opt "Module"
      (fun () ->
         s_ "CVS requires a named module, as each CVS server can host \
             multiple named repositories. (__mandatory__ for CVS)")
      (fun (_, src_repo) -> src_repo.src_repo_module)
  in
  let branch =
    new_field_opt "Branch"
      (fun () ->
         s_ "Define a meaningful branch for this repository.")
      (fun (_, src_repo) -> src_repo.src_repo_branch)
  in
  let tag =
    new_field_opt "Tag"
      (fun () ->
         s_ "Identify a state corresponding to this particular package \
             version.")
      (fun (_, src_repo) -> src_repo.src_repo_tag)
  in
  let subdir =
    new_field_opt "Subdir"
      (fun () ->
         s_ "Define the relative path from the root of the repository to the \
             top directory for the package, i.e. the directory containing the \
             package's `_oasis` file.")
      (fun (_, src_repo) -> src_repo.src_repo_subdir)
  in
  (* TODO: enforce contraint about module/branch/subdir/tag depending
   * in src_repo_type
  *)
  schm,
  (fun features_data nm data ->
     SrcRepo
       (cmn_section_gen features_data nm data,
        {
          src_repo_type        = typ data;
          src_repo_location    = location data;
          src_repo_browser     = browser data;
          src_repo_module      = modul data;
          src_repo_branch      = branch data;
          src_repo_tag         = tag data;
          src_repo_subdir      = subdir data;
        }))
