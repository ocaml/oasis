
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValues;;
open OASISUtils;;
open CommonGettext;;
open PropList.Field;;

let schema, generator =
  let schm =
    schema "Library"
  in
  let path =
    new_field schm "Path" 
      directory
      (fun () ->
         s_ "Directory containing the library")
  in
  let modules =
    new_field schm "Modules" 
      ~default:[]
      ~quickstart_level:Beginner
      modules
      (fun () ->
         s_ "List of modules to compile.") 
  in
  let parent =
    new_field schm "Parent"
      ~default:None
      (opt internal_library)
      (fun () ->
         s_ "Library which includes the current library. The current library \
             will be built as its parents and installed along it.")
  in
  let findlib_name = 
    new_field schm "FindlibName"
      ~default:None
      (opt pkgname)
      (fun () ->
         s_ "Name used by findlib.")
  in
  let build, install, compiled_object = 
    std_field (s_ "library") Best schm
  in
  let build_depends, build_tools =
    depends_field schm
  in
  let c_sources = 
    c_field schm
  in
  let data_files = 
    data_field schm
  in
    schm,
    (fun (_: string) data ->
       {
         lib_build           = build data;
         lib_install         = install data;
         lib_path            = path data;
         lib_modules         = modules data;
         lib_compiled_object = compiled_object data;
         lib_build_depends   = build_depends data;
         lib_build_tools     = build_tools data;
         lib_c_sources       = c_sources data;
         lib_data_files      = data_files data;
         lib_parent          = parent data;
         lib_findlib_name    = findlib_name data;
         lib_schema_data     = data;
       })
;;

(** Library tree
  *)
type tree_t = 
  | Node of name * library * tree_t list
  | Leaf of name * library
;;

(** Compute groups of libraries, associate root libraries with 
    a tree of its children.
  *)
let groups libs =

  (** Associate a name with its children *)
  let children =
    List.fold_left
      (fun mp (nm, lib) ->
         match lib.lib_parent with 
           | Some p_nm ->
               begin
                 let children =
                   try 
                     MapString.find p_nm mp
                   with Not_found ->
                     []
                 in
                   MapString.add p_nm ((nm, lib) :: children) mp
               end
           | None ->
               mp)
      MapString.empty
      libs
  in

  (** Build library tree *)
  let rec tree_of_library nm lib =
      try 
        Node (nm, lib,
              List.rev_map 
                (fun (child_nm, child_lib) -> 
                   tree_of_library child_nm child_lib)
                (MapString.find nm children))
      with Not_found ->
        Leaf (nm, lib)
  in

    List.fold_left
      (fun acc (nm, lib) ->
         if lib.lib_parent = None then
           (tree_of_library nm lib) :: acc
         else
           acc)
      []
      libs
;;

(** Compute internal library findlib names, including subpackage
    and return a map of it.
  *)
let findlib_names libs = 

  (* Return the findlib name of the library without parents *)
  let findlib_name_no_recurse nm lib =
    (* Check that the name is correct *)
    pkgname.parse 
      (match lib.lib_findlib_name with 
         | Some nm -> nm
         | None -> nm)
  in

  (* Compute names in a tree *)
  let rec findlib_names_aux prt_name mp tree =
    let nm, cur_name =
      match tree with
        | Leaf (nm, lib) 
        | Node (nm, lib, _) ->
            begin
              let loc_name =
                findlib_name_no_recurse nm lib
              in
              let cur_name =
                match prt_name with 
                  | Some nm -> nm^"."^loc_name
                  | None -> loc_name
              in
                nm, cur_name
            end
    in
    let mp =
      MapString.add nm cur_name mp
    in 
      match tree with 
        | Leaf (_, _) ->
            mp
        | Node (_, _, children) ->
            List.fold_left
              (findlib_names_aux (Some cur_name))
              mp
              children
  in


    List.fold_left
      (findlib_names_aux None)
      MapString.empty
      (groups libs)

;;

