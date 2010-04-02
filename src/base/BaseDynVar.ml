
(** Dynamic variables which are set in setup.log during compilation
    @author Sylvain Le Gall
  
    This variables are typically executable real name that are initially not
    set and then are set while building.
  *)

open OASISTypes
open OASISGettext
open BaseEnv
open BaseBuilt

let init pkg =
  List.iter 
    (function
       | Executable (cs, bs, exec) ->
           var_ignore
             (var_define 
                (* We don't save this variable *)
                ~dump:false
                ~short_desc:(fun () -> 
                               Printf.sprintf 
                                 (f_ "Filename of executable '%s'")
                                 cs.cs_name)
                cs.cs_name
                (lazy 
                   (let fn_opt = 
                      fold
                        BExec cs.cs_name
                        (fun _ fn -> Some fn)
                        None
                    in
                      match fn_opt with
                        | Some fn -> fn
                        | None ->
                            raise 
                              (PropList.Not_set
                                 (cs.cs_name, 
                                  Some (Printf.sprintf 
                                          (f_ "Executable '%s' not yet built.")
                                          cs.cs_name))))))

       | Library _ | Flag _ | Test _ | SrcRepo _ | Doc _ ->
           ())
    pkg.sections

(* END EXPORT *)
