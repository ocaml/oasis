
(** {1 Checking for particular features} 
  *)

open BaseEnv

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  var_redefine
    prg 
    (lazy 
       (let alternate = 
          List.fold_left 
            (fun res e ->
               match res with 
                 | Some _ -> 
                     res
                 | None ->
                     try
                       Some (BaseFileUtil.which e)
                     with Not_found ->
                       None)
            None
            prg_lst
        in
          match alternate with
            | Some prg -> prg
            | None -> raise Not_found))

(** Check the presence of a particular program.
  *)
let prog prg =
  prog_best prg [prg]

(** Check the presence of a program or its native version
  *)
let prog_opt prg = 
  prog_best prg [prg^".opt"; prg]

let ocamlfind = 
  prog "ocamlfind"

(** Check version, following Sys.ocaml_version convention
  *)
let version 
      var_prefix 
      (str_cmp, cmp, var_cmp) 
      fversion 
      () = 
  (* Really compare version provided *)
  let var = 
    var_prefix^"_version_"^var_cmp
  in
    var_redefine 
      ~hide:true 
      var
      (lazy
         (let version =
            match fversion () with 
              | "[Distributed with OCaml]" ->
                  begin
                    try 
                      (var_get "ocaml_version")
                    with Not_found ->
                      BaseMessage.warning 
                        "Variable ocaml_version not defined, fallback to default";
                      Sys.ocaml_version
                  end
              | res ->
                  res
          in
            prerr_endline version;
            if BaseVersion.comparator_apply version cmp then
              version
            else
              failwith 
                (Printf.sprintf
                   "Cannot satisfy version constraint on %s: %s (version: %s)"
                   var_prefix
                   str_cmp
                   version)))
      ()

(** Check for findlib package
  *)
let package ?version_comparator pkg () =
  let var =
    let buff = 
      Buffer.create ((String.length pkg) + 4)
    in
      Buffer.add_string buff "pkg_";
      String.iter
        (fun c ->
           if ('a' <= c && c <= 'z') 
             || 
              ('A' <= c && c <= 'Z') 
             || 
              ('0' <= c && c <= '9')
             ||
              c = '_' then
             Buffer.add_char buff c
           else
             Buffer.add_char buff '_')
        pkg;
      Buffer.contents buff
  in
  let findlib_dir pkg = 
    let dir = 
      BaseExec.run_read_one_line
        (ocamlfind ())
        ["query"; "-format"; "%d"; pkg]
    in
      if Sys.is_directory dir then
        dir
      else
        failwith
          (Printf.sprintf
             "When looking for findlib package %s, \
              directory %s return doesn't exist"
             pkg dir)
  in
  let findlib_version pkg =
    BaseExec.run_read_one_line 
      (ocamlfind ())
      ["query"; "-format"; "%v"; pkg]
  in
  let vl =
    var_redefine
      var
      (lazy (findlib_dir pkg))
      ()
  in
    (
      match version_comparator with 
        | Some ver_cmp ->
            var_ignore
              (version 
                 var
                 ver_cmp
                 (fun _ -> findlib_version pkg))
        | None -> 
            ()
    );
    vl
