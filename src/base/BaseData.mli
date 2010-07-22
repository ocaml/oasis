(** Exported modules for embedding 
  
    The whole module is {b not exported}. It is auto-generated using other
    modules.
  *)

(** All exported modules from base library, default
    content for 'setup.ml'.
  *)
val basesys_ml: string

(** Minimal set of exported modules to load the 'setup.data'
    files. Use to create OCaml script that will use 'setup.data'. 
    Example auto-generated 'myocamlbuild.ml' contains this set.
  *)
val basesysenvironment_ml: string
