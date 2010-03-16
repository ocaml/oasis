
(** Register files built to be installed
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISGettext

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
           BaseMessage.warning 
             (Printf.sprintf
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
                   nm));
           acc
         end)
    acc
    (BaseLog.filter
       [to_log_event t nm])

