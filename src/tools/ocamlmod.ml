

let dump_ml chn_out fn =
  let () = 
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
    let export_end =
      Str.regexp "(\\* +END +EXPORT +\\*)"
    in
    let export_ended =
      ref false
    in
      (
        try
          while not !export_ended do
            let line = 
              input_line chn_in
            in
              if Str.string_match export_end line 0 then
                export_ended := true
              else
                Printf.fprintf chn_out "  %s\n" line
          done
        with End_of_file ->
          ()
      );
      close_in chn_in
  in

    Printf.fprintf chn_out "module %s =\n" modname;
    Printf.fprintf chn_out "struct\n";
    Printf.fprintf chn_out "# 1 %S\n" fn;
    export_extract chn_out fn;
    Printf.fprintf chn_out "end;;\n\n";
;;

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
;;

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
;;

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
;;

