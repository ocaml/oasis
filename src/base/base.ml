
(** Autobuild basic operation.
    @author Sylvain Le Gall
  *)


(** {1 Types} *)

(** Type to describe comment *)
type comment_format =
    {
      comment_begin: string;
      comment_end:   string;
    }
;;

(** Type to describe file generation *)
type file_format = 
    {
      fn:             string;
      tags:           (string * string) list;
      header:         string list;
      contents:       string list list;
      footer:         string list;
      comment:        comment_format;
    }
;;

(** Context of execution *)
type context =
    {
      verbose:       bool;
      create_backup: bool;
    }
;;

(** {1 Information and status} *)

(** Standard message *)
let info ctxt msg =
  if ctxt.verbose then
    prerr_endline msg
;;

(** Warning *)
let warn msg =
  prerr_endline msg
;;

(** Signal error and exit *)
let error msg =
  prerr_endline msg;
  exit 1
;;

(** {1 Utils} *)


(** Remove trailing whitespace *)
let strip_whitespace str =
  let strlen = 
    String.length str
  in
  let is_whitespace =
    function 
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false
  in 
  let skip_beg =
    let idx =
      ref 0
    in
      while !idx < strlen && is_whitespace str.[!idx] do 
        incr idx 
      done;
      !idx
  in
  let skip_end =
    let idx = 
      ref ((String.length str) - 1)
    in
      while !idx >= 0 && is_whitespace str.[!idx] do
        decr idx
      done;
      !idx
  in
    if skip_beg <= skip_end then
      String.sub str skip_beg (skip_end - skip_beg + 1)
    else
      ""
;;

(** Compute digest of a string list *)
let digest_list lst =
  Digest.to_hex (Digest.string (String.concat " " lst))
;;

(** Replace an association in a list *)
let replace_assoc nm vl lst =
  (nm, vl) :: (List.remove_assoc nm lst)
;;

(** {1 Comments} *)

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

let comment_makefile = comment_sh
;;

let comment_bat = 
  {
    comment_begin = "rem";
    comment_end   = "";
  }
;;

(** Extract comment *)
let comment_extract cmt line = 
  let comment_begin_length = 
    String.length cmt.comment_begin
  in
  let comment_end_length = 
    String.length cmt.comment_end
  in
  let comment_min_length = 
   comment_begin_length + comment_end_length 
  in
  let is_comment =
    (* Enough data ? *)
    (String.length line) >= comment_min_length &&
     (* Begin line comment ? *)
     (String.sub 
        line 
        0 
        comment_begin_length) = cmt.comment_begin &&
     (* End line comment ? *)
     (String.sub 
        line 
        ((String.length line) - comment_end_length) 
        comment_end_length) = cmt.comment_end 
  in
    if is_comment then 
      (
        Some 
          (String.sub
             line
             comment_begin_length 
             ((String.length line) - comment_min_length)
          )
      )
    else 
      (
        None
      )
;;

(** Find special comments, related to file generation *)
let special_comment_extract cmt line = 
  let comment_extracted = 
    comment_extract cmt line 
  in
    match comment_extracted with 
      | Some str ->
          (
            let is_special_comment = 
              String.length str > 1 &&
              str.[0] = '+'
            in
              if is_special_comment then 
                Some
                  (String.sub 
                     str 
                     1 
                     ((String.length str) - 1)
                  )
              else 
                None
          )
      | None ->
          (
            None
          )
;;

(** Extract special comments information *)
let tags_extract = 
  let lexer = 
    Genlex.make_lexer 
      [
        "tag"; 
        ","; 
        "AUTOBUILD_START"; 
        "AUTOBUILD_STOP"
      ]
  in
    fun cmt line tags ->
      (
        match special_comment_extract cmt line with 
          | Some str ->
              (
                let strm =
                  Stream.of_string str
                in
                let strm_token =
                  lexer strm
                in
                let njunk n strm = 
                  for i = 1 to n do
                    Stream.junk strm_token
                  done
                in
                let rec list_extract acc = 
                  match Stream.npeek 3 strm_token with 
                    | [Genlex.Ident nm; Genlex.String vl; Genlex.Kwd ","] ->
                        njunk 3 strm_token;
                        list_extract ((nm, vl) :: acc)
                    | Genlex.Ident nm :: Genlex.String vl :: _ ->
                        njunk 2 strm_token;
                        (nm, vl) :: acc
                    | _ ->
                        error "Syntax error in tag list"
                in
                let rec tags_extract_aux cmt_beg cmt_end tags = 
                  try 
                    match Stream.next strm_token with 
                      | Genlex.Kwd "tag" ->
                          let ntags = 
                            list_extract tags 
                          in
                            tags_extract_aux cmt_beg cmt_end ntags
                      | Genlex.Kwd "AUTOBUILD_START" ->
                          tags_extract_aux true cmt_end tags
                      | Genlex.Kwd "AUTOBUILD_STOP" ->
                          tags_extract_aux cmt_beg true tags
                      | _ ->
                          (* Skip token *)
                          tags_extract_aux cmt_beg cmt_end tags
                  with Stream.Failure ->
                    Some (cmt_beg, cmt_end, (List.rev tags))
                in
                  tags_extract_aux false false tags
              )
          | None ->
              None
      )
;;

(** {1 File generation} *)

let file_dbug ff =
  prerr_endline ("File: "^ff.fn);
  prerr_endline "Header:";
  List.iter prerr_endline ff.header;
  prerr_endline ("Tags: "^
                 (String.concat 
                    ", " 
                    (List.map 
                       (fun (a, b) -> a^" "^b) 
                       ff.tags)));
  prerr_endline "Contents:";
  List.iter prerr_endline (List.flatten ff.contents);
  prerr_endline "Footer:";
  List.iter prerr_endline ff.footer
;;

let file_read fn cmt = 
  let fd =
    open_in fn
  in

  (* Read file contents *)
  let contents = 
    let acc =
      ref []
    in
    let () =
      try 
        while true do 
          acc := (input_line fd) :: !acc
        done
      with End_of_file ->
        ()
    in
      List.rev !acc
  in

     
  (* Extract different parts of the file *)
  let rec extract_footer _ lst =
    lst
  in
  let rec extract_contents acc tags lst =
    (* Due to the fact that start tag can also be end tag, the first line 
     * must be the start/end tag line
     *)
    match lst with 
      | line :: tl ->
          (
            match tags_extract cmt line tags with 
              | Some (_, last, ntags) ->
                  (
                      if last then
                        (
                          let footer =
                            extract_footer [] tl
                          in
                            (List.rev ntags), (List.rev acc), footer
                        )
                      else 
                        (
                          (* Just skip the line *)
                          extract_contents acc ntags tl
                        )

                  )
              | None ->
                  (
                    extract_contents (line :: acc) tags tl 
                  )
          )
      | [] ->
          (
            warn ("Cannot find AUTOBUILD-STOP in file '"^fn^"'");
            (List.rev tags), (List.rev acc), []
          )
  in
  let rec extract_header acc lst =
    match lst with 
      | line :: tl ->
          (
            match tags_extract cmt line [] with 
              | Some (true, last, tags) ->
                  (
                    let ntags, content, footer = 
                      if last then
                        (List.rev tags), [], (extract_footer [] tl)
                      else
                        extract_contents [] tags tl
                    in
                      (List.rev acc), ntags, content, footer
                  )
              | _ ->
                  (
                    extract_header (line :: acc) tl
                  )
          )
      | [] ->
          (
            warn ("Cannot find AUTOBUILD-START in file '"^fn^"'");
            (List.rev acc), [], [], []
          )
  in

  (* Extract data from file *)
  let (header, tags, content, footer) = 
    extract_header [] contents
  in
    {
      fn       = fn;
      header   = header;
      tags     = tags;
      contents = [content];
      footer   = footer;
      comment  = cmt;
    }
;;

let file_generate ctxt ff = 
  let digest_var part = 
    part^"_digest"
  in
  let ff_merged = 
    if Sys.file_exists ff.fn then
      (
        let ff_real = 
          file_read ff.fn ff.comment 
        in
        let exist_digest part tags = 
          try
            ignore(List.assoc (digest_var part) tags);
            true
          with Not_found ->
            false
        in
        let check_digest part tags lst =
          try 
            let expected_digest =
              List.assoc (digest_var part) tags
            in
            let real_digest = 
              digest_list lst
            in
              if real_digest = expected_digest then
                (
                  true
                )
              else
                (
                  warn ("Digest for "^part^
                        " has changed, not updating this part");
                  false
                )
          with Not_found ->
            false
        in
        let update part tags lst new_lst =
          if lst != new_lst && new_lst != [] then
            (
              if lst = [] then
                (
                  info ctxt (part^" in file "^ff.fn^
                             " is empty, replacing by automatic content.");
                  new_lst,
                  replace_assoc 
                    (digest_var part) 
                    (digest_list new_lst) 
                    tags

                )
              else if (exist_digest part tags) && (check_digest part tags lst) then
                (
                  info ctxt ("Updating "^part^" in file "^ff.fn);
                  new_lst,
                  replace_assoc 
                    (digest_var part) 
                    (digest_list new_lst) 
                    tags
                )
              else
                (
                  lst, tags
                )
            )
          else
            (
              info ctxt ("Nothing to update for "^part^" in file "^ff.fn);
              lst, tags
            )
        in
        let nheader, ntags =
          update 
            "header" 
            ff_real.tags 
            ff_real.header 
            ff.header
        in
        let nfooter, nntags =
          update
            "footer"
            ntags
            ff_real.footer
            ff.footer
        in
        let () = 
          if not (exist_digest "content" nntags) || 
             check_digest "content" nntags (List.flatten ff_real.contents) then
            ()
          else
            (
              if not ctxt.create_backup then
                error ("AUTOBUILD content of file "^ff.fn^
                       " has changed and no backup will be made")
            )
        in
          {
            ff with 
              header   = nheader;
              tags     = 
                replace_assoc 
                  (digest_var "content") 
                  (digest_list (List.flatten ff.contents))
                  nntags;
              footer   = nfooter;
          }
      )
    else
      (
        let updated_tags = 
          [
            digest_var "header", digest_list ff.header;
            digest_var "footer", digest_list ff.footer;
            digest_var "content", digest_list (List.flatten ff.contents);
          ]
        in
        let ntags = 
          List.fold_left
            (fun tags (nm, vl) ->
               replace_assoc nm vl tags
            )
            ff.tags
            updated_tags
        in
          {
            ff with 
                tags = ntags
          }
      )
  in
  let () = 
    if ctxt.create_backup then
      Sys.rename ff_merged.fn (ff_merged.fn^".bak")
  in
  let fd = 
    open_out ff_merged.fn
  in
  let output_list =
    List.iter (fun str -> output_string fd str; output_string fd "\n") 
  in
  let output_special_comment str = 
    output_string fd ff_merged.comment.comment_begin;
    output_string fd "+ ";
    output_string fd str;
    output_string fd " ";
    output_string fd ff_merged.comment.comment_end;
    output_string fd "\n"
  in
  let output_tag lst =
    List.iter 
      (fun (nm, vl) -> 
         output_special_comment 
           (Printf.sprintf "tag %s %S" nm vl)
      )
      lst
  in
    output_list ff_merged.header;
    output_special_comment "AUTOBUILD_START";
    output_special_comment "DO NOT EDIT THIS PART";
    output_tag ff_merged.tags;
    output_list (List.flatten ff_merged.contents);
    output_special_comment "AUTOBUILD_STOP";
    output_list ff_merged.footer
;;

(** {1 Create base system} *)

let packages = ["findlib"; "fileutils"];;

let buildsys_sh pkg = 
  {
    fn       = "buildsys.sh";
    header   = ["#!/bin/sh"];
    tags     = [];
    contents = 
      [
        [
          Printf.sprintf 
            "PACKAGES=\"$PACKAGES %s\""
            (String.concat " " pkg)
        ];
        BaseData.buildsys_sh
      ];
    footer   = [];
    comment  = comment_sh;
  }
;;

let buildsys_bat pkg =
  {
    fn       = "buildsys.bat";
    header   = [];
    tags     = [];
    contents = [BaseData.buildsys_bat];
    footer   = [];
    comment  = comment_bat;
  }
;;

let makefile = 
  {
    fn       = "Makefile";
    header   = [];
    tags     = [];
    contents = [BaseData.makefile];
    footer   = [];
    comment  = comment_makefile;
  }
;;

let buildsys_ml = 
  {
    fn       = "buildsys.ml";
    header   = [];
    tags     = [];
    contents = [BaseData.buildsys_ml];
    footer   = [];
    comment  = comment_ml;
  }
;;

let main ctxt pkg = 
  file_generate ctxt (buildsys_sh  pkg);
  file_generate ctxt (buildsys_bat pkg);
  file_generate ctxt makefile;
  file_generate ctxt buildsys_ml
;;
