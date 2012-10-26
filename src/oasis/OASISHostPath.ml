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


open Filename

module Unix = OASISUnixPath


let os_type_windows = Sys.os_type = "Win32"

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
              (fun b -> a,b)
              lst2)
         lst1)
  in

  let rec combined_paths lst =
    match lst with
      | p1 :: p2 :: tl ->
          let acc =
            (List.map
               (fun (a,b) -> Filename.concat a b)
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
      (fun (p,e) ->
         if String.length e > 0 && e.[0] <> '.' then
           p ^ "." ^ e
         else
           p ^ e)
      ((combined_paths paths) * exts)
  in
    List.find
      (if case_sensitive then
         file_exists_case
       else
         Sys.file_exists)
      alternatives

let which prg =
  let path_sep =
    match os_type_windows with
      | true -> ';'
      | false  -> ':'
  in
  let path_lst = OASISString.nsplit (Sys.getenv "PATH") path_sep in
  let exec_ext =
    match os_type_windows with
      | true ->  "" :: (OASISString.nsplit (Sys.getenv "PATHEXT") path_sep)
      | false -> [""]
  in
  find_file ~case_sensitive:false [path_lst; [prg]] exec_ext


let use_cygwin =
  match os_type_windows with
  | false -> false
  | true  ->
    try
      begin match Sys.getenv "OASIS_USE_CYGWIN" with
      | "" | "No" | "no" | "NO" | "false" | "FALSE" | "False" -> false
      | _ -> true
      end
    with
    | Not_found ->
      try
        let _cp = which "cp"
        and _rm = which "rm"
        and _bash = which "bash"
        and _mkdir = which "mkdir" in
        true
      with
      | Not_found -> false

(* generic quote and unixquote are taken from ocaml source *)
let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  Buffer.add_char b '\'';
  for i = 0 to l - 1 do
    if s.[i] = '\''
    then Buffer.add_string b quotequote
    else Buffer.add_char b  s.[i]
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

let unixquote = generic_quote "'\\''"

let quote = if use_cygwin then unixquote else Filename.quote


let get_naccu accu str first pos =
  (* I assume c//d is identic to c/d
   * the only exception (Network devices \\xyz\asdf)
   * is covered below
   *)
  if first = pos then
    accu
  else
    let nlen = pos - first in
    let nstr = String.sub str first nlen in
    (* test/././ is the same as test *)
    if nlen = 1 && nstr = "." then
      accu
    (* a/b/../ is the same as a *)
    else if nlen = 2 && nstr = ".." then
      match accu with
      | []      -> [ nstr ]
      | ".."::_ -> nstr::accu
      | hd::tl  -> tl
    else
      nstr::accu

let is_path_sep = function
| '\\' | '/' -> true
| _ -> false

(* - strips trailing slashes (exceptions in case of C:\ and / )
 * - enforce uniform path seperators
 *)

let uniform_path path_sep = function
| "" -> "" (* ??? *)
| str ->
  let rec iter accu str len first pos =
    if pos >= len then
      List.rev (get_naccu accu str first pos)
    else
      let next = succ pos in
      match is_path_sep str.[pos] with
      | true -> iter (get_naccu accu str first pos) str len next next
      | false -> iter accu str len first next
  in
  let is_unix_root = is_path_sep str.[0]
  and len = String.length str in
  let next_sep = len > 1 && is_path_sep str.[1] in
  let is_network_root = is_unix_root && next_sep
  and is_currel = str.[0] = '.' && ( next_sep || len = 1 ) in
  let l = iter [] str len 0 0 in
  (* trailing slashes are normally stripped.
   * this is not possible in case of root folders
   *)
  let l_min = match l with
  | [] -> [ "" ]
  | _ -> l
  in
  let l =
    if is_network_root then
      ""::""::l_min
    else if is_unix_root then
      ""::l_min
    else if is_currel then
      "."::l
    else
      match l with
      | s :: [] ->
        (* root folders like C:\ *)
        if String.length s = 2 && s.[1] = ':' then
          s :: [ "" ]
        else
          l
      | _ -> l
  in
  String.concat path_sep l

let make =
  function
    | [] ->
        invalid_arg "OASISHostPath.make"
    | hd :: tl ->
        List.fold_left concat hd tl

let of_unix str =
  match os_type_windows with
  | false -> str
  | true  ->
    let path_sep = match use_cygwin with
    | true ->  "/"
    | false -> "\\"
    in
    uniform_path path_sep str


(* END EXPORT *)

let compare fn1 fn2 =
  let fn_norm fn =
    match OASISPath_intern.fn_reduce []
            (OASISPath_intern.fn_reader fn) with
      | (`RootRelative str) :: tl ->
          (`Root str) :: `CurrentDir :: tl
      | lst ->
          lst
  in
  let fn_string =
    function
      | `Root str
      | `RootRelative str
      | `Component str ->
          str
      | `CurrentDir ->
          "."
      | `ParentDir ->
          ".."
  in
  let rec compare' =
    function
      | (hd1 :: tl1), (hd2 :: tl2) ->
          if hd1 = hd2 then
            compare' (tl1, tl2)
          else
            String.compare (fn_string hd1) (fn_string hd2)
      | [], [] ->
          0
      | _ :: _, [] ->
          1
      | [], _ :: _ ->
          -1
  in
    compare' (fn_norm fn1, fn_norm fn2)

let add_extension fn ext =
  fn^"."^ext
