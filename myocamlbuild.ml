
open Ocamlbuild_plugin;;
open Command;;

let ocamlify = A"ocamlify";;

rule "ocamlify: %.mlify.depends %.mlify -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mlify"; "%.mlify.depends"]
  begin 
    fun env build ->
      let depends = 
        env "%.mlify.depends"
      in
      let mlify = 
        env "%.mlify"
      in
      let ml =
        env "%.ml"
      in
      let depends_lst = 
        let deps = 
          ref []
        in
        let fd = 
          open_in depends
        in
          (
            try
              while true; do
                deps := (input_line fd) :: !deps
              done;
            with End_of_file ->
              ()
          );
          List.rev !deps
      in
      let () = 
        List.iter
          (function
             | Outcome.Good _ -> ()
             | Outcome.Bad exn -> raise exn
          ) 
          (build [depends_lst])
      in
      Cmd(S[ocamlify; 
            T(tags_of_pathname mlify++"ocamlify"++"compile");
            A"--output"; P(ml);
            P(mlify)])
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
            A"--output"; P(env "%.mlify.depends"); 
            P(env "%.mlify");])
  end
;;
