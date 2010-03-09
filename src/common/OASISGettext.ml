
(** Gettext interface
  *)

let s_ str = 
  str

let f_ (str : ('a, 'b, 'c) format) =
  str


(* END EXPORT *)

include
  Gettext.Program
    (struct
       let textdomain   = "oasis"
       let codeset      = None
       let dir          = None
       let dependencies = Gettext.init
     end)
    (GettextCamomile.Map)
