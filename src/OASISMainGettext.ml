
(** Main gettext interfaces
  *)

open OASISGettext

let init = 
  [], ""

IFDEF HAS_GETTEXT THEN
include
  Gettext.Program
    (struct
       let textdomain   = "oasis"
       let codeset      = None
       let dependencies = Gettext.init @ OASISGettext.init
       let dir = 
         try 
           Some (Sys.getenv "OASIS_GETTEXT_DIR")
         with Not_found ->
           None
     end)
    (GettextStub.Native)
ENDIF

