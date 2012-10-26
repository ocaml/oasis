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

open OASISGettext

let file_exists_case = OASISHostPath.file_exists_case
let find_file = OASISHostPath.find_file
let which ~(ctxt:OASISContext.t) b = OASISHostPath.which b

let use_wintools = OASISHostPath.os_type_windows && not OASISHostPath.use_cygwin


(**/**)
let rec fix_dir dn =
  (* Windows hack because Sys.file_exists "src\\" = false when
   * Sys.file_exists "src" = true
   *)
  let ln =
    String.length dn
  in
    if use_wintools && ln > 0 && dn.[ln - 1] = '\\' then
      fix_dir (String.sub dn 0 (ln - 1))
    else
      dn

let q s = OASISHostPath.quote (OASISHostPath.of_unix s)

(**/**)

let cp ~ctxt ?(recurse=false) src tgt =
  if recurse then
    match use_wintools with
      | true -> OASISExec.run ~ctxt
            "xcopy" [q src; q tgt; "/E"]
      | false ->
          OASISExec.run ~ctxt
            "cp" ["-r"; q src; q tgt]
  else
    OASISExec.run ~ctxt
      (match use_wintools with
       | true -> "copy"
       | false -> "cp")
      [q src; q tgt]

let mkdir ~ctxt tgt =
  OASISExec.run ~ctxt
    (match use_wintools with
       | true -> "md"
       | false -> "mkdir")
    [q tgt]

let rec mkdir_parent ~ctxt f tgt =
  let tgt =
    fix_dir tgt
  in
    if Sys.file_exists tgt then
      begin
        if not (Sys.is_directory tgt) then
          OASISUtils.failwithf
            (f_ "Cannot create directory '%s', a file of the same name already \
                 exists")
            tgt
      end
    else
      begin
        mkdir_parent ~ctxt f (Filename.dirname tgt);
        if not (Sys.file_exists tgt) then
          begin
            f tgt;
            mkdir ~ctxt tgt
          end
      end

let rmdir ~ctxt tgt =
  if Sys.readdir tgt = [||] then
    begin
      match use_wintools with
        | true ->
            OASISExec.run ~ctxt "rd" [q tgt]
        | false ->
            OASISExec.run ~ctxt "rm" ["-r"; q tgt]
    end

let glob ~ctxt fn =
 let basename =
   Filename.basename fn
 in
   if String.length basename >= 2 &&
      basename.[0] = '*' &&
      basename.[1] = '.' then
     begin
       let ext_len =
         (String.length basename) - 2
       in
       let ext =
         String.sub basename 2 ext_len
       in
       let dirname =
         Filename.dirname fn
       in
         Array.fold_left
           (fun acc fn ->
              try
                let fn_ext =
                  String.sub
                    fn
                    ((String.length fn) - ext_len)
                    ext_len
                in
                  if fn_ext = ext then
                    (Filename.concat dirname fn) :: acc
                  else
                    acc
              with Invalid_argument _ ->
                acc)
           []
           (Sys.readdir dirname)
     end
   else
     begin
       if file_exists_case fn then
         [fn]
       else
         []
     end
