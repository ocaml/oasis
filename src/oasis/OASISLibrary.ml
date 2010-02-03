
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISUtils
open CommonGettext

(** Library group are organized in trees
  *)
type group_t = 
  | Container of findlib_name * (group_t list)
  | Package of findlib_name * name * library * (group_t list)

(** Compute groups of libraries, associate root libraries with 
    a tree of its children. A group of libraries is defined by 
    the fact that these libraries has a parental relationship 
    and must be isntalled together, with the same META file.
  *)
let group_libs libs =
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

  (* Compute findlib name of a single node *)
  let findlib_name nm lib =
    match lib.lib_findlib_name with 
      | Some nm -> nm
      | None -> nm
  in

  (** Build a package tree *)
  let rec tree_of_library containers nm lib =
    match containers with
      | hd :: tl ->
          Container (hd, [tree_of_library tl nm lib])
      | [] ->
          (* TODO: allow merging containers with the same 
           * name 
           *)
          Package 
            (findlib_name nm lib,
             nm, 
             lib, 
             (try 
                List.rev_map 
                  (fun (child_nm, child_lib) -> 
                     tree_of_library 
                       child_lib.lib_findlib_containers 
                       child_nm 
                       child_lib)
                  (MapString.find nm children)
              with Not_found ->
                []))
  in

    (* TODO: check that libraries are unique *)
    List.fold_left
      (fun acc (nm, lib) ->
         if lib.lib_parent = None then
           (tree_of_library 
              lib.lib_findlib_containers 
              nm 
              lib) :: acc
         else
           acc)
      []
      libs

(** Compute internal library findlib names, including subpackage
    and return a map of it.
  *)
let findlib_name_map libs = 

  (* Compute names in a tree *)
  let rec findlib_names_aux path mp grp =
    let fndlb_nm, children, mp =
      match grp with
        | Container (fndlb_nm, children) ->
            fndlb_nm, children, mp
                                  
        | Package (fndlb_nm, nm, _, children) ->
            fndlb_nm, children, (MapString.add nm (path, fndlb_nm) mp)
    in
    let fndlb_nm_full =
      (match path with
         | Some pth -> pth^"."
         | None -> "")^
      fndlb_nm
    in
      List.fold_left
        (findlib_names_aux (Some fndlb_nm_full))
        mp
        children
  in

    List.fold_left
      (findlib_names_aux None)
      MapString.empty
      (group_libs libs)


(** Return the findlib name of the library without parents *)
let findlib_of_name ?(recurse=false) map nm =
  try 
    let (path, fndlb_nm) = 
      MapString.find nm map
    in
      match path with 
        | Some pth when recurse -> pth^"."^fndlb_nm
        | _ -> fndlb_nm

  with Not_found ->
    failwith 
      (Printf.sprintf
         (f_ "Unable to translate internal library '%s' to findlib name")
         nm)

(** Return the findlib root name of a group, it takes into account
    containers. So the return group name is the toplevel name
    for both libraries and theirs containers.
  *)
let findlib_of_group = 
  function
    | Container (fndlb_nm, _) 
    | Package (fndlb_nm, _, _, _) -> fndlb_nm

(** Return the root library, i.e. the first found into the group tree
    that has no parent.
  *)
let root_of_group grp =
  let rec root_lib_aux =
    function 
      | Container (_, children) ->
          root_lib_lst children        
      | Package (_, nm, lib, children) ->
          if lib.lib_parent = None then 
            nm, lib
          else
            root_lib_lst children
  and root_lib_lst =
    function
      | [] ->
          raise Not_found
      | hd :: tl ->
          try
            root_lib_aux hd
          with Not_found ->
            root_lib_lst tl
  in
    try
      root_lib_aux grp
    with Not_found ->
      failwith
        (Printf.sprintf 
           "Unable to determine root library of findlib library '%s'"
           (findlib_of_group grp))

(* END EXPORT *)

open OASISSchema
open OASISValues
open CommonGettext
open PropList.Field

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
      (* TODO: Check that the name is correct if this value is None, the
               package name must be correct 
       *)
      (opt pkgname)
      (fun () ->
         s_ "Name used by findlib.")
  in
  let findlib_containers =
    new_field schm "FindlibContainers"
      ~default:[]
      (* TODO: check that a container doesn't overwrite a real package 
       *)
      (dot_separated string_not_empty)
      (fun () ->
         s_ "Virtual containers for sub-package, dot-separated")
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
         lib_build              = build data;
         lib_install            = install data;
         lib_path               = path data;
         lib_modules            = modules data;
         lib_compiled_object    = compiled_object data;
         lib_build_depends      = build_depends data;
         lib_build_tools        = build_tools data;
         lib_c_sources          = c_sources data;
         lib_data_files         = data_files data;
         lib_parent             = parent data;
         lib_findlib_name       = findlib_name data;
         lib_findlib_containers = findlib_containers data;
         lib_schema_data        = data;
       })

