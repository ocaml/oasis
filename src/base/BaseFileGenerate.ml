
(** Generate files with auto-generated part
    @author Sylvain Le Gall
  *)

open BaseMessage;;

(** {1 Comments} *)

(** Type to describe comment *)
type comment_format =
    {
      of_string: string -> string;
      regexp:    string -> Str.regexp;
      start:     string;
      stop:      string;
    }
;;

type content =
  (* Use header * body * footer without start/stop comment *)
  | Split of (string list) * (string list) * (string list)
  (* Look for start/stop comment and split *)
  | NeedSplit of string list
;;

(**/**)
let (start_msg, stop_msg) =
  "AUTOBUILD_START",
  "AUTOBUILD_STOP"
;;

let white_space =
  "[ \t]*"
;;

let comment cmt_beg cmt_end =
  let of_string =
      match cmt_end with
        | None ->
            Printf.sprintf "%s %s" cmt_beg 
        | Some cmt_end ->
            (fun str ->
               Printf.sprintf "%s %s %s" cmt_beg str cmt_end)
  in
  let regexp str = 
    match cmt_end with 
      | Some cmt_end ->
          Str.regexp ("^"^white_space^
                      (Str.quote cmt_beg)^
                      white_space^
                      (Str.quote str)^
                      white_space^
                      (Str.quote cmt_end)^
                      white_space^"$")
      | None ->
          Str.regexp ("^"^white_space^
                      (Str.quote cmt_beg)^
                      white_space^
                      (Str.quote str)^
                      white_space^"$")
  in
    {
      of_string = of_string;
      regexp    = regexp;
      start     = of_string start_msg;
      stop      = of_string stop_msg;
    }
;;

(**/**)

let comment_ml =
  comment "(*" (Some "*)")
;;

let comment_sh = 
  comment "#" None
;;

let comment_makefile = 
  comment_sh
;;

let comment_ocamlbuild =
  comment_sh
;;

let comment_bat = 
  comment "rem" None
;;

(** Generate a file using a template. Only the part between AUTOBUILD_START and 
    AUTOBUILD_END will really be replaced if the file exist. If file doesn't exist
    use the whole template.
 *)
let file_generate ?(target) fn comment content = 

  (* Match start and stop comment
   *)
  let is_start, is_stop =
    let match_regexp msg =
      let rgxp =
        comment.regexp msg
      in
        fun str ->
          Str.string_match rgxp str 0
    in
      (match_regexp start_msg),
      (match_regexp stop_msg)
  in

  let do_not_edit =
    comment.regexp "DO NOT EDIT (digest: \\(.*\\))"
  in

  (* Compute the digest of a list
   *)
  let digest_of_list lst =
    Digest.to_hex 
      (Digest.string 
         (String.concat "\n" lst))
  in

  (* Write body, header and footer to output file. Separate
   * each part by appropriate comment and digest.
   *)
  let output_file lst_header lst_body lst_footer =
    let chn_out =
      open_out_bin 
        (match target with 
           | Some fn -> fn
           | None    -> fn)
    in
    let output_line str =
      output_string chn_out str;
      output_char   chn_out '\n'
    in
    let output_lst =
      List.iter
        output_line
    in

      output_lst   lst_header;
      output_line  comment.start;
      output_line 
        (comment.of_string 
           (Printf.sprintf 
              "DO NOT EDIT (digest: %s)"
              (digest_of_list lst_body)));
      output_lst   lst_body;
      output_line  comment.stop;
      output_lst   lst_footer;
      close_out chn_out
  in

  (* Separate a list into three part: header, body and footer.
   * Each part is separated by the appropriate start/stop comment.
   *)
  let split_header_body_footer lst = 
    (* Extract elem until the first that match condition.
     * The element that matched is removed 
     *)
    let rec split_cond cond acc lst =
      match lst with 
        | hd :: tl ->
            if cond hd then
              split_cond cond (hd :: acc) tl
            else
              (List.rev acc), tl
        | [] ->
            raise Not_found
    in
      (* Begin by extracting header, if that fail there
       * is no body/footer.
       *)
      try
        let lst_header, tl =
          split_cond 
            (fun str -> not (is_start str))
            []
            lst
        in
        let lst_body, lst_footer =
          try
            split_cond 
              (fun str -> not (is_stop str))
              []
              tl
          with Not_found ->
            tl, []
        in
          lst_header, Some (lst_body, lst_footer)
      with Not_found ->
        lst, None
  in

  (* Split content
   *)
  let content_header,
      content_body,
      content_footer =

    match content with
      | Split (lst_header, lst_body, lst_footer) ->
          lst_header, lst_body, lst_footer
      | NeedSplit lst ->
          (
            match split_header_body_footer lst with
              | lst_header, Some (lst_body, lst_footer) ->
                  lst_header, lst_body, lst_footer
              | lst_header, None ->
                  warning 
                    (Printf.sprintf 
                       "No replace section found in template for file %s" 
                       fn);
                  lst_header, [], []
          )
  in

    if Sys.file_exists fn then
      (
        let lst_fn =
          let chn_in =
            open_in_bin fn 
          in
          let lst =
            ref []
          in
            (
              try
                while true do
                  lst := (input_line chn_in) :: !lst
                done
              with End_of_file ->
                ()
            );
            close_in chn_in;
            List.rev !lst
        in

        (* Actual split file content
         *)
          match split_header_body_footer lst_fn with 
            | fn_header, Some (fn_body, fn_footer) ->
                (
                  (* Strip "do not digest" message
                   *)
                  let fn_body =
                    match fn_body with
                      | hd :: tl when Str.string_match do_not_edit hd 0 -> 
                          (
                            let expected_digest =
                              Str.matched_group 1 hd
                            in
                            let digest =
                              digest_of_list tl
                            in
                              if expected_digest <> digest then 
                                failwith 
                                  (Printf.sprintf 
                                     "Digest sum for replace section of file %s \
                                      has changed (%s <> %s). Remove digest line first!"
                                     fn
                                     expected_digest
                                     digest)
                              else
                                tl
                          )
                      | lst ->
                          lst
                  in
                    (* Regenerate if required *)
                    if target <> None ||
                       (fn_body <> content_body) then
                      (
                        info (Printf.sprintf "Regenerating file %s" fn);
                        output_file 
                          fn_header
                          content_body
                          fn_footer
                      )
                    else
                      (
                        info (Printf.sprintf "Nothing to update for file %s" fn);
                      )
                )
            | fn_header, None ->
                (
                  info (Printf.sprintf "No replace section in file %s" fn);
                  match target with
                    | Some fn ->
                        (
                          (* We still generate file since it is not source file *)
                          let chn =
                            open_out_bin fn
                          in
                            List.iter
                              (fun str ->
                                 output_string chn str;
                                 output_char   chn '\n')
                              fn_header;
                            close_out chn
                        )
                    | None ->
                        ()
                )
      )
    else
      (
        info (Printf.sprintf "File %s doesn't exist, creating it." fn);
        output_file 
          content_header 
          content_body
          content_footer
      )
;;
