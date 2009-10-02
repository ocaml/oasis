
(** Configure using ocaml-autobuild internal scheme
    @author Sylvain Le Gall
  *)

module Msg = BaseMessage;;
open BaseEnvironment;;

(** Build environment using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg_name pkg_version args checks ab_files env argv =

  List.iter
    (fun (nm, vl) -> var_set nm vl env)
    [
      "pkg_name", (lazy pkg_name);
      "pkg_version", (lazy pkg_version);
    ];

  (* Parse command line *)
  BaseArgExt.parse argv (BaseArgExt.default :: args) env;

  (* Do some check *)
  BaseCheck.run checks env;

  (* Replace data in file *)
  BaseFileAB.replace ab_files env;

  dump env;
  print env
;;
