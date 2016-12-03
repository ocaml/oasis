open Ocamlbuild_plugin;;
open Command;;

(* TODO: use an ocamlbuild plugin for ocamlify. *)

let depends_from_file env build ?(fmod=fun x -> x) fn =
  let depends_lst =
    let deps = ref [] in
    let fd = open_in  fn in
    begin
      try
        while true; do deps := (fmod (input_line fd)) :: !deps done
      with End_of_file ->
        close_in fd
    end;
    List.rev !deps
  in
    List.iter
      (fun fn ->
         List.iter
           (function
              | Outcome.Good _ -> ()
              | Outcome.Bad exn ->
                  prerr_endline
                    (Printf.sprintf
                       "Could not build '%s': %s"
                       fn
                       (Printexc.to_string exn));

                  raise exn
           )
           (build [[fn]])
      )
      depends_lst
;;

let ocamlify = A"ocamlify";;

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

rule "ocamlify: %.mlify & %.mlify.depends -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mlify"; "%.mlify.depends"]
  begin
    fun env build ->
      depends_from_file
        env
        build
        (env "%.mlify.depends");
      Cmd(S[ocamlify; A"--output"; P(env "%.ml"); P(env "%.mlify")])
  end
;;

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin;;

dispatch dispatch_default;;
