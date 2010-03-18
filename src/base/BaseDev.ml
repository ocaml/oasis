
(** Entry points for setup.ml in dev mode
  *)

TYPE_CONV_PATH "BaseDev"

open OASISGettext

type t = 
    {
      oasis_cmd:  string;
      oasis_args: string list;
      self_fn:    string;
    } with odn

let update_and_run t = 
  let dev_fn = 
    "setup-dev.ml"
  in

  (* Command to run after creation of dev_fn *)
  let bootstrap_ocaml = 
    Sys.executable_name
  in
  let bootstrap_args =
    Array.to_list
      (Array.map
         (fun a ->
            if a = t.self_fn then
              dev_fn
            else
              a)
         Sys.argv)
  in

  let safe_exit () = 
    if Sys.file_exists dev_fn then
      Sys.remove dev_fn
  in

    if Sys.file_exists dev_fn then
      OASISMessage.error
        (f_ "File %s already exists, cannot generate it for \
             dev-mode. Please remove it first.")
        dev_fn;

    try 
      (* Run OASIS to generate a temporary setup.ml
       *)
      BaseExec.run 
        t.oasis_cmd 
        ("-quiet" :: "-setup-fn" :: dev_fn :: t.oasis_args);
      (* Run own command line by replacing setup.ml by 
       * setup-dev.ml
       *)
      BaseExec.run
        bootstrap_ocaml
        bootstrap_args;

      (* Clean dev file *)
      safe_exit ()

    with e ->
      safe_exit ();
      raise e

(* END EXPORT *)

let create setup_fn = 
  let args =
    let rec filter_opt =
      function 
        | "-setup-fn" :: _ :: lst 
        | "-dev" :: lst ->
            filter_opt lst
        | hd :: tl ->
            hd :: (filter_opt tl)
        | [] ->
            []
    in
      (* Remove exec name = Sys.argv.(0) *)
      List.tl 
        (filter_opt 
           (Array.to_list Sys.argv))
  in
    {
      oasis_cmd  = Sys.executable_name;
      oasis_args = args;
      self_fn    = setup_fn;
    }
