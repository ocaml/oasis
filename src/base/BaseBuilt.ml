
(** Register files built to be installed
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISGettext
open BaseStandardVar

type filenames = filename list

type t =
  | BExec    (* Executable *)
  | BExecLib (* Library coming with executable *)
  | BLib     (* Library *)
  | BDoc     (* Documentation *)

let to_log_event t nm = 
  "built_"^
  (match t with 
     | BExec -> "exec"
     | BExecLib -> "exec_lib"
     | BLib -> "lib"
     | BDoc -> "doc")^
  "_"^nm

(* Register files built *)
let register t nm lst = 
  List.iter
    (fun fn ->
       BaseLog.register 
         (to_log_event t nm)
         fn)
    lst

(* Unregister all files built *)
let unregister t nm =
  List.iter
    (fun (e, d) ->
       BaseLog.unregister e d)
    (BaseLog.filter [to_log_event t nm])

(* Fold-left files built, filter existing
   and non-existing files.
 *)
let fold t nm f acc = 
  List.fold_left 
    (fun acc (_, fn) ->
       if Sys.file_exists fn then
         begin
           f acc fn
         end
       else
         begin
           OASISMessage.warning 
             (f_ "File '%s' has been marked as built \
                for %s but doesn't exist")
             fn
             (Printf.sprintf
                (match t with 
                   | BExec | BExecLib -> 
                       (f_ "executable %s")
                   | BLib ->
                       (f_ "library %s")
                   | BDoc ->
                       (f_ "documentation %s"))
                nm);
           acc
         end)
    acc
    (BaseLog.filter
       [to_log_event t nm])

(** Generate the list of files that should be built for 
    an executable.
  *)
let of_executable ffn (cs, bs, exec) = 
  let unix_exec_is, unix_dll_opt = 
    OASISExecutable.unix_exec_is 
      (cs, bs, exec)
      (fun () -> 
         bool_of_string 
           (is_native ()))
      ext_dll
  in
  let evs = 
    (BExec, cs.cs_name, [ffn unix_exec_is])
    ::
    (match unix_dll_opt with
       | Some fn ->
           [BExecLib, cs.cs_name, [ffn fn]]
       | None ->
           [])
  in
    evs,
    unix_exec_is,
    unix_dll_opt

(** Generate the list of files that should be built for
    a library
  *)
let of_library ffn (cs, bs, lib) = 
  let unix_lst = 
    OASISLibrary.generated_unix_files
      (cs, bs, lib)
      (fun fn ->
         Sys.file_exists (BaseFilePath.of_unix fn))
      (fun () ->
         bool_of_string (is_native ()))
      ext_lib
      ext_dll
  in
  let evs =
    [BLib,
     cs.cs_name,
     List.map ffn unix_lst]
  in
    evs, unix_lst

(* Unregister all build event related to a package
 *)
let clean_all pkg =
    List.fold_left
      (fun () ->
         function
           | Library (cs, _, _) -> 
               unregister BLib cs.cs_name
           | Executable (cs, _, _) ->
               unregister BExec cs.cs_name;
               unregister BExecLib cs.cs_name
           | Flag _ | SrcRepo _ | Doc _ | Test _ ->
               ())
      ()
      pkg.sections
