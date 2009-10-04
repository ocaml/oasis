
(** Configure using ocaml-autobuild internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnvironment;;
open BaseExpr;;

(** Build environment using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg_name pkg_version flags checks ab_files env argv =

  (** Initialize flags *)
  List.iter 
    (fun (nm, hlp, choices) ->
       let apply ?short_desc () = 
           var_set
             ?short_desc
             ~cli:CLIAuto
             ODefault
             nm 
             (lazy (choose choices env))
             env
       in
         match hlp with 
           | Some hlp ->
               apply ~short_desc:hlp ()
           | None ->
               apply ())
    (("pkg_name",    Some "Package name", (singleton pkg_name)) :: 
     ("pkg_version", Some "Package version", (singleton pkg_version)) ::
     flags);

  (** Initialize standard variables *)
  List.iter 
    (fun v -> var_ignore (v env))
    BaseStandardVar.all;

  (* Parse command line *)
  BaseArgExt.parse argv (args env) env;

  (* Do some check *)
  BaseCheck.run checks env;

  (* Replace data in file *)
  BaseFileAB.replace ab_files env;

  dump env;
  print env
;;
