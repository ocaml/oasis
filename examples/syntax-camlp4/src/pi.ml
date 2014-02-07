open Camlp4.PreCast
open Syntax

EXTEND Gram
  GLOBAL: expr;

  expr: LEVEL "simple" [[ "PI" -> <:expr< 3.1415926 >> ]];

END
