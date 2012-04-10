type t = 
  | Or of t * t  
  | And of t * t
  | License of string * (string option)
