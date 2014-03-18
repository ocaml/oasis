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


open Filename


module Unix = OASISUnixPath

let bash_cmd = ref ( fun () -> "" )

let use_bash () = ( !bash_cmd () ) <> ""


(* generic quote and unixquote are taken from ocaml source *)
let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
    Buffer.add_char b '\'';
    for i = 0 to l - 1 do
      if s.[i] = '\'' then
        Buffer.add_string b quotequote
      else
        Buffer.add_char b  s.[i]
    done;
    Buffer.add_char b '\'';
    Buffer.contents b

let unixquote = generic_quote "'\\''"

let win = Sys.os_type = "Win32"

let quote str =
  if win && use_bash () then
    unixquote str
  else
    quote str

(* uniform_path (only called, if Sys.os_type = "Win32")
 * - enforces uniform path seperators
 * - strips trailing slashes (exceptions in case of C:\ and / )
 * - removes (some) unnecessary file components like ./././
 *)

let get_naccu accu str first pos =
  (* I assume c//d is identic to c/d
   * the only exception (Network devices \\xyz\asdf)
   * is covered in uniform_path
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


let uniform_path path_sep = function
  | "" -> "" (* Raise an exception? Or an possible intermediate result?
             * Filename.basename and dirname also don't raise exceptions *)
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
        let is_unix_root = is_path_sep str.[0] in
        let len = String.length str in
        let next_sep = len > 1 && is_path_sep str.[1] in
        let is_network_root = is_unix_root && next_sep in
        let is_currel = str.[0] = '.' && ( next_sep || len = 1 ) in
        let l = iter [] str len 0 0 in
        (* Trailing slashes are normally stripped.
         * This is not possible in case of root folders
         * Sys.file_exists "C:" is false, Sys.file_exists "C:\\" true
         *)
        let l_min =
          match l with
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
                  if String.length s = 2 && s.[1] = ':' &&
                    len > 2 && is_path_sep str.[2]
                  then
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
        List.fold_left Filename.concat hd tl


let of_unix str =
  if win = false then
    str
  else
    let path_sep =
      if use_bash () then
        "/"
            else
        "\\"
    in
      uniform_path path_sep str



(* see findlib's src/findlib/frontend.ml for details *)
let ocamlfind_unquote dir =
  match Sys.os_type with
    | "Cygwin"
    | "Win32" ->
      let len = String.length dir in
        if len < 3 || dir.[0] <> '"' || dir.[len - 1] <> '"' ||
          String.contains dir ' ' = false then
          dir
        else
          String.sub dir 1 (len - 2)
    | _ -> dir


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
