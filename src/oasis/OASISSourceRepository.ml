
(** SourceRepository schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open CommonGettext
open PropList.Field

let schema, generator =
  let schm =
    schema "SourceRepository"
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
      (fun () -> s_ "VCS type")
  in
  let location =
    new_field schm "Location"
      url
      (fun () ->
         s_ "URL of the repository. The exact form of this field depends on \
             the repository type.")
  in
  let browser =
    new_field schm "Browser"
      ~default:None
      (opt url)
      (fun () ->
         s_ "URL where the repository can be navigated using a web browser.")
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
  in
  let branch =
    new_field_opt "Branch"
      (fun () -> 
         s_ "Define a meaningful branch for this repository.")
  in
  let tag =
    new_field_opt "Tag"
      (fun () -> 
         s_ "Identify a state corresponding to this particular package version")
  in
  let subdir = 
    new_field_opt "Subdir"
      (fun () ->
         s_ "Define the relative path from the root of the repository to the \
             top directory for the package, i.e. the directory containing the \
             package's `_oasis` file.")
  in
    schm,
    (fun (_: string) data ->
       {
         src_repo_type        = typ data;
         src_repo_location    = location data;
         src_repo_browser     = browser data;
         src_repo_module      = modul data;
         src_repo_branch      = branch data;
         src_repo_tag         = tag data;
         src_repo_subdir      = subdir data;
         src_repo_schema_data = data;
       })
