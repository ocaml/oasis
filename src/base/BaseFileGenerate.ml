
(** Generate files with auto-generated part
    @author Sylvain Le Gall
  *)

open BaseMessage;;

(** {1 Comments} *)

(** Type to describe comment *)
type comment_format =
    {
      comment_begin: string;
      comment_end:   string;
    }
;;

let comment_ml =
  {
    comment_begin = "(*";
    comment_end   = "*)";
  }
;;

let comment_sh = 
  {
    comment_begin = "#";
    comment_end   = "";
  }
;;

let comment_makefile = 
  comment_sh
;;

let comment_bat = 
  {
    comment_begin = "rem";
    comment_end   = "";
  }
;;

(** Generate a file using a template. Only the part between AUTOBUILD_START and 
    AUTOBUILD_END will really be replaced if the file exist. If file doesn't exist
    use the whole template.
 *)
let file_generate ?(target) fn content_lst comment = 
  let {comment_begin = comment_begin; comment_end = comment_end} =
    comment
  in

  let start_replace =
    comment_begin ^ " AUTOBUILD_START " ^ comment_end
  in

  let do_not_edit =
    Str.regexp (comment_begin ^ " DO NOT EDIT (digest: \\(.*\\)) " ^ comment_end)
  in

  let stop_replace =
    comment_begin ^ " AUTOBUILD_STOP " ^ comment_end
  in

  let output_file buff_begin buff_replace buff_end =
    let chn_out =
      open_out_bin 
        (match target with 
           | Some fn -> fn
           | None    -> fn)
    in
      Buffer.output_buffer chn_out buff_begin;
      output_string chn_out start_replace;
      output_char   chn_out '\n';
      output_string chn_out 
        (Printf.sprintf 
           "%s DO NOT EDIT (digest: %s) %s\n"
           comment_begin 
           (Digest.to_hex (Digest.string (Buffer.contents buff_replace)))
           comment_end);
      Buffer.output_buffer chn_out buff_replace;
      output_string chn_out stop_replace;
      output_char   chn_out '\n';
      Buffer.output_buffer chn_out buff_end;
      close_out chn_out
  in


  let template_begin,
      template_replace,
      template_end =

    let buff_begin,
        buff_replace,
        buff_end = 
      Buffer.create 13,
      Buffer.create 13,
      Buffer.create 13
    in

    let buff_cur =
      ref buff_begin
    in
    let section_replace_found =
      ref false
    in

      List.iter 
        (fun str ->
           if str = start_replace then
             (
               buff_cur := buff_replace;
               section_replace_found := true
             )
           else if str = stop_replace then
             (
               buff_cur := buff_end
             )
           else
             (
               Buffer.add_string !buff_cur str;
               Buffer.add_char   !buff_cur '\n'
             ))
        content_lst;

      if not !section_replace_found then
        warning 
          (Printf.sprintf 
             "No replace section found in template for file %s" 
             fn);

      buff_begin, buff_replace, buff_end
  in

  let input_line_cond buff chn cond = 
    let str =
      input_line chn
    in
      if cond str then
        (
          Buffer.add_string buff str;
          Buffer.add_char buff '\n';
          true
        )
      else
        (
          false
        )
  in


    if Sys.file_exists fn then
      (
        let chn_in =
          open_in_bin fn 
        in
        let size_in =
          in_channel_length chn_in
        in
        let buff_begin, 
            buff_replace,
            buff_end = 
          Buffer.create size_in,
          Buffer.create size_in,
          Buffer.create size_in
        in

        let no_replace_section =
          (* Find begin of replace section *)
          try
            while 
              input_line_cond
                buff_begin
                chn_in 
                (fun str -> str <> start_replace) do
              ()
            done;
            false
          with End_of_file ->
            true
        in

          if not no_replace_section then
            (
              let read_replace () = 
                (* Read the whole replace section *)
                try
                  while 
                    input_line_cond
                      buff_replace
                      chn_in
                      (fun str -> str <> stop_replace) do
                    ()
                  done
                with End_of_file ->
                  ()
              in

              let check_digest =
                try
                  let str =
                    input_line chn_in 
                  in
                    (* Following line should be "DO NOT EDIT" + digest *)
                    if Str.string_match do_not_edit str 0 then
                      (
                        let res = 
                          Some (Str.matched_group 1 str)
                        in
                          read_replace ();
                          res
                      )
                    else if str <> stop_replace then
                      (
                        (* The line is not what we expect, just
                         * add it to buffer replace 
                         *)
                        Buffer.add_string buff_replace str;
                        Buffer.add_char buff_replace '\n';
                        read_replace ();

                        (* Nothing to check *)
                        None
                      )
                    else 
                      (
                        (* Nothing to check *)
                        None
                      )
                with End_of_file ->
                  (
                    None
                  )
              in

                (* Verify digest of the replace section *)
                (
                  match check_digest with
                    | Some hex ->
                        let real_hex =
                          Digest.to_hex (Digest.string (Buffer.contents buff_replace))
                        in
                          if hex <> real_hex then 
                            failwith 
                              (Printf.sprintf 
                                 "Digest sum for replace section of file %s \
                                  has changed (%s <> %s). Remove digest line first!"
                                 fn
                                 hex 
                                 real_hex)
                    | None ->
                        ()
                );

                (* Read until end of file *)
                (
                  try
                    while 
                      input_line_cond
                        buff_end
                        chn_in
                        (fun _ -> true) do
                      ()
                    done
                  with End_of_file ->
                    ()
                );

                close_in chn_in;

                (* Regenerate if required *)
                if target <> None ||
                   ((Buffer.contents template_replace) 
                      <> (Buffer.contents buff_replace)) then
                  (
                    info (Printf.sprintf "Regenerating file %s" fn);
                    output_file buff_begin template_replace buff_end
                  )
                else
                  (
                    info (Printf.sprintf "Nothing to update for file %s" fn);
                  )
            )
          else
            (
              info (Printf.sprintf "No replace section in file %s" fn);
              match target with
                | Some fn ->
                    (
                      (* We still generate file since it is not source file *)
                      let chn =
                        open_out_bin fn
                      in
                        Buffer.output_buffer chn buff_begin;
                        close_out chn
                    )
                | None ->
                    ()
            )
      )
    else
      (
        info (Printf.sprintf "File %s doesn't exist, creating it." fn);
        output_file template_begin template_replace template_end
      )
;;
