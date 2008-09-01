
open Ocamlbuild_plugin;;
open Command;;

let ocamlify = A"ocamlify";;

rule "ocamlify: %.mlify.depends %.mlify -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mlify"; "%.mlify.depends"]
  begin 
    fun env _ -> 
      Cmd(S[ocamlify; 
            T(tags_of_pathname (env "%.mlify")++"ocamlify"++"compile");
            A"--output"; P(env "%.ml");
            P(env "%.mlify")])
  end
;;

rule "ocamlify: %.mlify -> %.mlify.depends"
  ~prod:"%.mlify.depends"
  ~dep:"%.mlify"
  begin
    fun env _ -> 
      Cmd(S[ocamlify; 
            T(tags_of_pathname (env "%.mlify")++"ocamlify"++"depends");
            A"--depends"; 
            A"--target"; P(env "%.ml"); 
            A"--output"; P(env "%.mlify.depends"); 
            P(env "%.mlify");])
  end
;;

dispatch 
  begin 
    function
      | After_rules ->
          (* Depends *)
          dep
            ["compile"; "file:src/base/baseData.mlify"]
            ["src/base/buildSys.ml"];
      | _ -> ()
  end
;;
