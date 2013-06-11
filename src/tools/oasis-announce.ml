(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

#!/usr/bin/ocamlrun ocaml

(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;

#use "topfind";;
#require "oasis";;
#require "oasis.base";;
#require "pcre";;

open OASISMessage
open OASISTypes

let () = 
  let ctxt = 
    {!OASISContext.default with 
         OASISContext.ignore_plugins = true}
  in
  let pkg = 
    OASISParse.from_file
      ~ctxt
      "_oasis"
  in
  let version = 
    OASISVersion.string_of_version 
      pkg.version
  in

  let from = 
    "Sylvain Le Gall <sylvain@le-gall.net>"
  in

  let email_to = 
    "caml-list@inria.fr"
  in
  let email_bcc = 
    ""
  in

  let subject = 
    Printf.sprintf "[ANN] %s v%s: %s" pkg.name version pkg.synopsis
  in

  let body = 
    match pkg.description with 
      | Some txt -> txt
      | None -> 
          warning ~ctxt "No description";
          ""
  in

  let body = 
    match pkg.homepage with 
      | Some url ->
          body^(Printf.sprintf "\n\nHomepage:\n%s" url)
      | None ->
          warning ~ctxt "No homepage";
          body
  in

  let body = 
    let data =
      List.fold_left
        (fun data ->
           function 
             | SrcRepo (cs, srcrepo) when cs.cs_name = "head" ->
                 Some (cs, srcrepo)
             | _ ->
                 data)
        None
        pkg.sections
    in
      match data with
        | Some (cs, src) ->
            begin
              let get = 
                match src.src_repo_type with 
                  | Darcs -> 
                     Printf.sprintf "$ darcs get %s" src.src_repo_location
                  | Svn ->
                     Printf.sprintf "$ svn co %s" src.src_repo_location
                  | _ ->
                     failwith "Unsupported VCS"
              in
                body^"\n\nGet source code:\n"^get^
                (match src.src_repo_browser with
                   | Some url ->
                       "\n\nBrowse source code:\n"^url
                   | None ->
                       "")
            end

        | None ->
            warning ~ctxt "No source repository";
            body
  in

  let email = 
    Printf.sprintf
      "From: %s\n\
       To: %s\n\
       Bcc: %s\n\
       Subject: %s\n\
       \n\
       %s\n\
       \n\
       [Generated by 'OASIS announce']"

      from email_to email_bcc subject
      body 
  in

  let editor = 
    try 
      Sys.getenv "EDITOR"
    with Not_found ->
      "editor"
  in

  let mta = 
    try 
      Sys.getenv "OASIS_MTA"
    with Not_found ->
      "/usr/sbin/sendmail"
  in

  let fn = 
    Filename.temp_file "oasis-announce-" ".txt"
  in
    try 
      let () = 
        let chn_out = open_out fn in
          output_string chn_out email; 
          close_out chn_out
      in
      let edit_exit_code = 
        Sys.command 
          (Printf.sprintf "%s %s" 
             (Filename.quote editor) 
             (Filename.quote fn))
      in
      let send_announcement = 
        print_string "Send the announcement? (y/N) ";
        match read_line () with 
          | "y" -> true
          | _ -> false
      in
        if send_announcement then
          begin
            let mta_exit_code =
              assert(edit_exit_code = 0);
              Sys.command 
                (Printf.sprintf "%s -t < %s"
                   (Filename.quote mta)
                   (Filename.quote fn))
            in
              assert(mta_exit_code = 0);
              print_endline "Announcement sent."
          end
        else
          begin
            print_endline "No announcement sent."
          end;
        Sys.remove fn
    with e ->
      Sys.remove fn;
      raise e
