
(** Tool to transform a source module into an OCaml module
    @author Sylvain Le Gall
  *)

let dump_ml chn_out fn =
  (* filename that should be used to point to source file *)
  let real_fn =
    let pwd = 
      FileUtil.pwd () 
    in
    let pwd =
      (* Translate file from build directory to source
       * directory
       *)
      if FilePath.basename pwd = "_build" then
        FilePath.dirname pwd
      else
        pwd
    in
      FileUtil.readlink (FilePath.make_absolute pwd fn)
  in

  let () = 
    (* Warn if not a .ml file *)
    if not (Filename.check_suffix fn ".ml") then
      prerr_endline ("'"^fn^"' doesn't end with .ml")
  in

  let modname =
    String.capitalize 
      (Filename.basename
         (Filename.chop_suffix fn ".ml"))
  in

  let export_extract chn_out fn = 
    let chn_in =
      open_in fn 
    in
    let with_odn = 
      Str.regexp "with +odn\\($\\| \\)"
    in
    let type_conv_path =
      Str.regexp "TYPE_CONV_PATH +\"[^\"]*\""
    in
    let export_end =
      Str.regexp "(\\* +END +EXPORT +\\*)"
    in
    let export_start = 
      Str.regexp "(\\* +START +EXPORT +\\*)"
    in
    let export =
      ref true
    in
    let line_num = 
      ref 0
    in
    let mark_source_line () =
      Printf.fprintf chn_out "# %d %S\n" !line_num real_fn
    in
      (
        mark_source_line ();
        try
          while true do
            let line = 
              incr line_num;
              input_line chn_in
            in
              if Str.string_match export_end line 0 then
                begin
                  export := false;
                end

              else if Str.string_match export_start line 0 then
                begin
                  export := true;
                  mark_source_line ()
                end

              else if !export then  
                begin
                  let line = 
                    (* Remove ODN elements *)
                    List.fold_left
                      (fun str rgxp -> Str.global_replace rgxp "" str)
                      line
                      [with_odn; type_conv_path]
                  in
                    Printf.fprintf chn_out "  %s\n" line
                end
          done
        with End_of_file ->
          ()
      );
      close_in chn_in
  in

    Printf.fprintf chn_out "module %s = struct\n" modname;
    export_extract chn_out fn;
    Printf.fprintf chn_out "end\n\n"

let process chn_in curdir chn_out =
  try
    while true do 
      let fn =
        input_line chn_in
      in
      let real_fn =
        if Filename.is_relative fn then
          Filename.concat curdir fn
        else
          fn
      in
        if not (Sys.file_exists real_fn) then
          failwith ("Cannot find file '"^real_fn^"'");
        dump_ml chn_out real_fn
    done
  with End_of_file ->
    ()

let process_modfile fn =
  let chn_out =
    open_out ((Filename.chop_extension fn)^".ml")
  in
  let chn_in =
    open_in fn
  in
    process chn_in (Filename.dirname fn) chn_out;
    close_in chn_in;
    close_out chn_out

let () = 
  let lst =
    ref []
  in
    Arg.parse
      []
      (fun fn -> lst := fn :: !lst)
      "OCaml module generator written by Sylvain Le Gall";

    List.iter
      process_modfile 
      (List.rev !lst)
