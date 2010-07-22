(** Dynamic variables sets using 'setup.log'
  
    This variables are typically executable real name that are initially not
    set and then are set while building. They are computed and set once the 
    matching executable had been successfully created.

    @author Sylvain Le Gall
  *)

(** Read 'setup.log' and sets variables.
  *)
val init : OASISTypes.package -> unit
