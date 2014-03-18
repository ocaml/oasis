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


open OASISGettext


let file_exists_case fn =
  let dirname = Filename.dirname fn in
  let basename = Filename.basename fn in
    if Sys.file_exists dirname then
      if basename = Filename.current_dir_name then
        true
      else
        List.mem
          basename
          (Array.to_list (Sys.readdir dirname))
    else
      false


let find_file ?(case_sensitive=true) paths exts =

  (* Cardinal product of two list *)
  let ( * ) lst1 lst2 =
    List.flatten
      (List.map
         (fun a ->
            List.map
              (fun b -> a, b)
              lst2)
         lst1)
  in

  let rec combined_paths lst =
    match lst with
      | p1 :: p2 :: tl ->
          let acc =
            (List.map
               (fun (a, b) -> Filename.concat a b)
               (p1 * p2))
          in
            combined_paths (acc :: tl)
      | [e] ->
          e
      | [] ->
          []
  in

  let alternatives =
    List.map
      (fun (p, e) ->
         if String.length e > 0 && e.[0] <> '.' then
           p ^ "." ^ e
         else
           p ^ e)
      ((combined_paths paths) * exts)
  in
    List.find (fun file ->
      (if case_sensitive then
         file_exists_case file
       else
         Sys.file_exists file)
      && not (Sys.is_directory file)
    ) alternatives


let which ~ctxt prg =
  let path_sep =
    match Sys.os_type with
      | "Win32" ->
          ';'
      | _ ->
          ':'
  in
  let path_lst = OASISString.nsplit (Sys.getenv "PATH") path_sep in
  let exec_ext =
    match Sys.os_type with
      | "Win32" ->
          "" :: (OASISString.nsplit (Sys.getenv "PATHEXT") path_sep)
      | _ ->
          [""]
  in
  let p =
    find_file ~case_sensitive:false [path_lst; [prg]] exec_ext
  in
    OASISHostPath.of_unix p


(**/**)
let rec fix_dir dn =
  (* Windows hack because Sys.file_exists "src\\" = false when
   * Sys.file_exists "src" = true
   *)
  let ln =
    String.length dn
  in
    if Sys.os_type = "Win32" && ln > 0 && dn.[ln - 1] = '\\' then
      fix_dir (String.sub dn 0 (ln - 1))
    else
      dn


let q s = OASISHostPath.quote (OASISHostPath.of_unix s)
(**/**)


let cp ~ctxt ?(recurse=false) src tgt =
  if recurse then
    (*
      'xcopy /E' and 'cp -r' don't have the same semantic!
      -dir2 does exist:
        xcopy dir dir2 /E -> copys content of dir into dir2 (dir2/dir doesn't exist!)
        cp -r dir dir2    -> copys dir to dir2 (dir2/dir does exist!)
      -dir2 doesn't exist:
        xcopy dir dir2 /E -> request on command line, if dir2 is an directory or file.
        cp -r dir dir2    -> dir and dir2 have the same content (dir2/dir doesn't exist)
      Probably, there are even more differences,....
    *)
    if Sys.os_type = "Win32" && OASISHostPath.use_bash () = false then
      let tgt_real =
        if Sys.file_exists tgt = false then
          begin
            OASISExec.run ~ctxt "md" [q tgt];
            tgt
          end
        else
          let base = Filename.basename src in
          if base = "." then
            tgt
          else
            let tgt_real = Filename.concat tgt base in
              if Sys.file_exists tgt_real = false then
                OASISExec.run ~ctxt "md" [q tgt_real];
            tgt_real
      in
      OASISExec.run ~ctxt "xcopy" [q src; q tgt_real; "/E" ; "/Q" ; "/Y"]
    else
      OASISExec.run ~ctxt "cp" ["-r"; q src; q tgt]
  else
    if Sys.os_type = "Win32" && OASISHostPath.use_bash () = false then
      OASISExec.run ~ctxt "copy" [q src; q tgt ; "/Y"]
    else
      OASISExec.run ~ctxt "cp" [q src; q tgt]


let mkdir ~ctxt tgt =
  OASISExec.run ~ctxt
    ( if Sys.os_type = "Win32" && not (OASISHostPath.use_bash ()) then
        "md"
      else
        "mkdir" )
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
  if Sys.readdir tgt = [||] then begin
    if Sys.os_type = "Win32" && OASISHostPath.use_bash () = false then
      OASISExec.run ~ctxt "rd" [q tgt]
    else
      OASISExec.run ~ctxt "rm" ["-r"; q tgt]
  end else begin
    OASISMessage.error ~ctxt
      (f_ "Cannot remove directory '%s': not empty.")
      tgt
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
