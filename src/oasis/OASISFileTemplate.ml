(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


open OASISMessage
open OASISGettext
open OASISUtils
open OASISTypes
open OASISString


type comment =
    {
      (** Return the string as a comment. *)
      of_string: string -> string;
      (** Return the string if it was able to strip comments from it. *)
      to_string: string -> string option;
      start:     string;
      stop:      string;
    }


type line = string


type body =
  | NoBody
  | Body of line list
  | BodyWithDigest of Digest.t * line list


type template =
    {
      fn:      host_filename;
      comment: comment;
      header:  line list;
      body:    body;
      footer:  line list;
      perm:    int;
    }


let start_msg = "OASIS_START"
let stop_msg  = "OASIS_STOP"


let comment cmt_beg cmt_end =
  let of_string =
      match cmt_end with
        | None ->
            Printf.sprintf "%s %s" cmt_beg
        | Some cmt_end ->
            (fun str ->
               Printf.sprintf "%s %s %s" cmt_beg str cmt_end)
  in
  let to_string str =
    try
      let str = trim str in
      let str = strip_starts_with ~what:cmt_beg str in
      let str =
        match cmt_end with
          | Some cmt_end ->
              strip_ends_with ~what:cmt_end str
          | None ->
              str
      in
        Some (trim str)
    with Not_found ->
        None
  in
    {
      of_string = of_string;
      to_string = to_string;
      start     = of_string start_msg;
      stop      = of_string stop_msg;
    }


let comment_ml =
  comment "(*" (Some "*)")


let comment_sh =
  comment "#" None


let comment_makefile =
  comment_sh


let comment_ocamlbuild =
  comment_sh


let comment_bat =
  comment "rem" None


let comment_meta =
  comment_sh


let template_make fn comment header body footer =
  {
    fn      = fn;
    comment = comment;
    header  = header;
    body    = Body body;
    footer  = footer;
    perm    = 0o644;
  }


let template_of_string_list ~ctxt ~template fn comment lst =

  (* Convert a Digest.to_hex string back into Digest.t *)
  let digest_of_hex s =
    let d       = String.make 16 '\000' in
    let hex_str = "0x00" in
      for i = 0 to (String.length d) - 1 do
        hex_str.[2] <- s.[2 * i];
        hex_str.[3] <- s.[2 * i + 1];
        d.[i] <- Char.chr (int_of_string hex_str)
      done;
      d
  in

  (* Match start and stop comment *)
  let is_start, is_stop =
    let test_comment msg str =
      match comment.to_string str with
        | Some msg' -> msg = msg'
        | None -> false
    in
      (test_comment start_msg),
      (test_comment stop_msg)
  in

  (* Match do not edit comment and extract digest. *)
  let extract_digest str =
    match comment.to_string str with
      | Some str' ->
          begin
            match tokenize ~tokens:["("; ")"; ":"] str' with
              | ["DO"; "NOT"; "EDIT"; "("; "digest"; ":"; digest; ")"] ->
                  digest
              | _ ->
                  raise Not_found
          end
      | None ->
          raise Not_found
  in

  (* Separate a list into three part: header, body and footer.
     Each part should be separated by the appropriate start/stop comment.
   *)
  let header, body, footer =
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
              (fun str ->
                if not (is_start str) then
                  begin
                    debug ~ctxt "Not start: %s" str;
                    true
                  end
                else
                  begin
                    debug ~ctxt "Start: %s" str;
                    false
                  end)
              []
              lst
          in
          let digest_body, tl =
            match tl with
              | (hd :: tl) as lst->
                  begin
                    try
                      Some (digest_of_hex (extract_digest hd)), tl
                    with Not_found ->
                      None, lst
                  end
              | lst ->
                  None, lst
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
            match digest_body with
              | Some d ->
                  lst_header, BodyWithDigest (d, lst_body), lst_footer
              | None ->
                  lst_header, Body lst_body, lst_footer

        with Not_found ->
          lst, NoBody, []

  in

  let res =
    template_make
      fn
      comment
      header
      []
      footer
  in

    if body = NoBody then
      warning ~ctxt
        (if template then
           (f_ "No replace section found in template for file %s")
         else
           (f_ "No replace section found in file %s"))
        fn;
    {res with body = body}


let template_of_file ~template fn comment =
 let lst =
   let chn_in =
     open_in_bin fn
   in
   let lst =
     ref []
   in
     begin
       try
         while true do
           lst := (input_line chn_in) :: !lst
         done
       with End_of_file ->
         ()
     end;
     close_in chn_in;
     List.rev !lst
 in
   template_of_string_list ~template fn comment lst


let template_of_mlfile fn header body footer  =

  let rec count_line str line_cur str_start =
    if str_start < String.length str then
      begin
        try
          count_line
            str
            (line_cur + 1)
            ((String.index_from str str_start '\n') + 1)
        with Not_found ->
          (line_cur + 1)
      end
    else
      begin
        line_cur + 1
      end
  in

  (* Make sure that line modifier contains reference to files that
   * really exist. If not modify the matching line.
   *)
  let check_line_modifier str =
    let found = ref false in
    let extract_line_modifier str =
      if starts_with "#" (trim str) then
        try
          match tokenize_genlex str with
            | [Genlex.Ident "#"; Genlex.Int _; Genlex.String fn] ->
                fn
            | _ ->
                raise Not_found
        with _ ->
          raise Not_found
      else
        raise Not_found
    in
    let lst =
      List.map
        (fun line ->
           try
             let windows_mode = ends_with ~what:"\r" line in
             let line_modifier_fn =
               extract_line_modifier line
             in
               found := true;
               if OASISFileUtil.file_exists_case line_modifier_fn then
                 (* We found a valid match, keep it *)
                 line
               (* The line modifier filename is not available, comment it. *)
               else if windows_mode then
                 (comment_ml.of_string (sub_end line 1))^"\r"
               else
                 comment_ml.of_string line

           with Not_found ->
             line)
        (split_newline ~trim:false str)
    in
      !found, (String.concat "\n" lst)
  in

  let insert_line_modifier lst line_start ~restore =
    let rlst, line_end =
      List.fold_left
        (fun (acc, line_cur) str ->
           (* Comment useless line modifier *)
           let contains_line_modifier, validated_str =
             check_line_modifier str
           in
           let line_cur =
             count_line validated_str line_cur 0
           in
             if contains_line_modifier then
               ((Printf.sprintf "# %d %S" line_cur fn) :: validated_str :: acc),
               (line_cur + 1)
             else
               (validated_str :: acc),
               line_cur)
        ([], line_start)
        lst
    in
    if restore then
      (* Insert a line modifier at the end for the following lines to
         refer to the original file lines -- important for footer
         errors in setup.ml or myocamlbuild.ml. *)
      let line_end = line_end + 1 in
      List.rev(Printf.sprintf "# %d %S" line_end fn :: rlst), line_end
    else
      List.rev rlst, line_end
  in

  let header, line_end =
    insert_line_modifier header 1 ~restore:false
  in

  let body, line_end =
    (* Will add 2 lines of comments: start + digest *)
    insert_line_modifier body (line_end + 2) ~restore:true
  in

  let footer, _ =
    if footer <> [] then
      (* Will add 1 line of comments: stop *)
      insert_line_modifier footer (line_end + 1) ~restore:false
    else
      [], line_end
  in

    template_make
      fn
      comment_ml
      header
      body
      footer


let digest_update t =
  {t with
       body =
         match t.body with
           | NoBody -> NoBody
           | BodyWithDigest (_, lst)
           | Body lst ->
               BodyWithDigest
                 (Digest.string (String.concat "\n" lst),
                  lst)}


let digest_check t =
  let t' = digest_update t in
    match t'.body, t.body with
      | BodyWithDigest (d', _), BodyWithDigest (d, _) ->
          d' = d
      | _, _ ->
          true


let merge t_org t_new =
  {t_new with
       header = t_org.header;
       body   =
         (if t_org.body = NoBody then
            t_org.body
          else
            t_new.body);
       footer = t_org.footer}


let to_file t =
  (* Be sure that digest match body content *)
  let t =
    digest_update t
  in

  (* Write body, header and footer to output file. Separate
   * each part by appropriate comment and digest.
   *)
  let chn_out =
    open_out_gen
      [Open_wronly; Open_creat; Open_trunc; Open_binary]
      t.perm
      t.fn
  in
  let output_line str =
    output_string chn_out str;
    output_char   chn_out '\n'
  in
  let output_lst =
    List.iter output_line
  in

    output_lst t.header;
    begin
      match t.body with
        | NoBody ->
            ()

        | BodyWithDigest (d, lst) ->
            output_line t.comment.start;
            output_line
              (t.comment.of_string
                 (Printf.sprintf
                    "DO NOT EDIT (digest: %s)"
                    (Digest.to_hex d)));
            output_lst   lst;
            output_line  t.comment.stop

        | Body lst ->
            output_line  t.comment.start;
            output_lst   lst;
            output_line  t.comment.stop
    end;
    output_lst t.footer;
    close_out chn_out;
    Unix.chmod t.fn t.perm


type file_generate_change =
  | Create of host_filename
  | Change of host_filename * host_filename option
  | NoChange


let file_rollback ~ctxt =
  function
    | Create fn ->
        info
          ~ctxt
          (f_ "Remove generated file '%s'")
          fn;
        (try Sys.remove fn with _ -> ())

    | Change (fn, Some bak) ->
        begin
          if Sys.file_exists bak then
            begin
              info ~ctxt (f_ "Restore file '%s' with backup file '%s'.")
                fn bak;
              Sys.rename bak fn
            end
          else
            begin
              warning ~ctxt
                (f_ "Backup file '%s' disappear, cannot restore \
                     file '%s'.")
                bak fn
            end
        end

    | Change (fn, None) ->
        warning ~ctxt
          (f_ "Cannot restore file '%s', no backup.")
          fn

    | NoChange ->
        ()


let file_generate ~ctxt ~backup t =

  (* Check that the files differ
   *)
  let body_has_changed t_org t_new =
    if t_org.body <> t_new.body then
      begin
        match t_org.body, t_new.body with
          | Body lst1, Body lst2
          | BodyWithDigest (_, lst1), BodyWithDigest (_, lst2)
          | BodyWithDigest (_, lst1), Body lst2
          | Body lst1, BodyWithDigest (_, lst2) ->
              begin
                let org_fn = String.concat "\n" lst1 in
                let new_fn = String.concat "\n" lst2 in
                  (String.compare org_fn new_fn) <> 0
              end

          | b1, b2 ->
              b1 <> b2
      end
    else
      false
  in

  (* Create a backup for a file and return its name
   *)
  let do_backup fn =
    let rec backup_aux =
      function
        | ext :: tl ->
            begin
              let fn_backup =
                fn ^ "." ^ ext
              in
                if not (Sys.file_exists fn_backup) then
                  begin
                    Sys.rename fn fn_backup;
                    OASISFileUtil.cp ~ctxt fn_backup fn;
                    fn_backup
                  end
                else
                  begin
                    backup_aux tl
                  end
            end

        | [] ->
            failwithf
              (f_ "File %s need a backup, but all filenames for \
                   the backup already exist")
              fn

    in
      backup_aux
        ("bak" ::
         (Array.to_list
            (Array.init 10 (Printf.sprintf "ba%d"))))
  in

    if Sys.file_exists t.fn then
      begin
        let t_org =
          template_of_file ~ctxt ~template:false t.fn t.comment
        in

          match t_org.body, body_has_changed t_org t with
            | NoBody, _ -> (* No body, nothing to do *)
                begin
                  NoChange
                end

            | _, true      (* Body has changed -> regenerate *)
            | Body _, _ -> (* Missing digest -> regenerate *)
                begin
                  (* Regenerate *)
                  let () =
                    info ~ctxt (f_ "Regenerating file %s") t.fn
                  in

                  let fn_backup =
                    (* Create a backup if required *)
                    if not (digest_check t_org) then
                      begin
                        let fn_bak =
                          do_backup t.fn
                        in
                          warning ~ctxt
                            (f_ "File %s has changed, doing a backup in %s")
                            t.fn fn_bak;
                          Some fn_bak
                      end
                    else if backup then
                      begin
                        Some (do_backup t.fn)
                      end
                    else
                      None
                  in
                    to_file (merge t_org t);
                    Change (t.fn, fn_backup)
                end

            | _, false -> (* No change *)
              begin
                info ~ctxt (f_ "File %s has not changed, skipping") t.fn;
                NoChange
              end
      end
    else
      begin
        info ~ctxt (f_ "File %s doesn't exist, creating it.") t.fn;
        to_file t;
        Create t.fn
      end


module S =
  Map.Make (
struct
  type t = host_filename

  let compare =
    OASISHostPath.compare
end)


exception AlreadyExists of host_filename


type templates = template S.t


let empty =
  S.empty


let find =
  S.find


let add e t =
  if S.mem e.fn t then
    raise (AlreadyExists e.fn)
  else
    S.add e.fn e t


let remove fn t =
  S.remove fn t


let replace e t =
  S.add e.fn e t


let fold f t acc =
  S.fold
    (fun k e acc ->
       f e acc)
    t
    acc
