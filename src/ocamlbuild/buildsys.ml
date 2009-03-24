
(** Ocamlbuild package
  *)
module OCamlbuild =
struct
  module Env = Environment
  module Chk = Check
  module Msg = Message
  module Pck = Pack

  (*
    
    {1 Ocamlbuild arguments}

   *)

  let args env = 
    Base.args env

  (* 
    
     {1 Ocamlbuild checks}
    
   *)

  let ocamlbuild = Chk.prog "ocamlbuild"
  
  (** Compute the default ocamlbuild flags (OS dependent)
    *)
  let ocamlbuild_flags =
    Env.cache "ocamlbuild_flags_full"
      (fun env ->
         Msg.checking "ocamlbuild flags";
         Msg.result_wrap 
           (match Sys.os_type with
              | "Win32" ->
                  "-classic-display -no-log -byte-plugin -install-lib-dir "^
                  (Filename.concat (Findlib.ocaml_stdlib ()) "ocamlbuild")
              | _ ->
                  ""
           ),
         env
      )
 
  (** Check for everything mandatory for building OCaml project with ocamlbuild
    *)
  let checks env =
    let nenv =
      Env.chain 
        [
          Base.checks;
          Chk.fenv ocamlbuild;
          Chk.fenv ocamlbuild_flags;
        ]
        env
    in
    let ocamlbest, nnenv = 
      Base.ocamlbest nenv
    in
      match ocamlbest with 
        | "opt" ->
            Env.var_add "ocamlbuild_best_library" "cmxa"
              (Env.var_add "ocamlbuild_best_program" "native" nnenv)
        | "byte"  ->
            Env.var_add "ocamlbuild_best_library" "cma"
              (Env.var_add "ocamlbuild_best_program" "byte" nnenv)
        | str ->
            failwith 
              ("Don't know what to do when ocamlbest is '"^str^"'")

  (*
    
     {1 Ocamlbuild targets}
    
   *)

  (** Execute ocamlbuild with given target 
    *)
  let ocamlbuild targets env =
    Action.exec 
      ("ocamlbuild" :: 
       (Env.var_get ~mandatory:true "ocamlbuild_flags_full" env) :: 
       targets) 
      env

  (** Compute targets for ocamlbuild
    *)
  let targets ocamlbuild_targets =
    Base.targets
    @
    [
      "all",
      (ocamlbuild ocamlbuild_targets);

      "clean",
      (ocamlbuild ["-clean"]);
    ]
    @
    (List.map
       (fun tgt -> tgt, (ocamlbuild [tgt]))
       ocamlbuild_targets
    )

  (*
   
     {1 Ocamlbuild .in files}
   
   *)

  let in_files =
     "myocamlbuild.ml.in" :: Base.in_files

  (*
   
     {1 Ocamlbuild package}
   
   *)

  let package ~ocamlbuild_targets =
    {
      Pck.args     = args;
      Pck.checks   = checks;
      Pck.in_files = in_files;
      Pck.targets  = targets ocamlbuild_targets;
    }

end
;;

