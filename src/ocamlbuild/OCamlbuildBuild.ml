
(** Runtime support for autobuild/OCamlbuild
    @author Sylvain Le Gall
  *)

let exec args =
  prerr_endline (String.concat " " args)
;;

let build target argv =
  exec ("ocamlbuild" :: target :: Array.to_list argv)
;;

let clean () = 
  exec ["ocamlbuild"; "-clean"]
;;

