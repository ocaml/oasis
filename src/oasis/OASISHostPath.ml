(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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
open OASISGettext


module Unix = OASISUnixPath


let make =
  function
    | [] ->
      invalid_arg "OASISHostPath.make"
    | hd :: tl ->
      List.fold_left Filename.concat hd tl


let of_unix ufn =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> ufn
  | "Win32" ->
    make
      (List.map
         (fun p ->
            if p = Unix.current_dir_name then
              current_dir_name
            else if p = Unix.parent_dir_name then
              parent_dir_name
            else
              p)
         (OASISString.nsplit ufn '/'))
  | os_type ->
    OASISUtils.failwithf
      (f_ "Don't know the path format of os_type %S when translating unix \
           filename. %S")
      os_type ufn


(* END EXPORT *)


let fn_norm fn =
  match OASISPath_intern.fn_reduce [] (OASISPath_intern.fn_reader fn) with
  | (`RootRelative str) :: tl -> (`Root str) :: `CurrentDir :: tl
  | lst -> lst


let compare fn1 fn2 =
  let fn_string =
    function
    | `Root str | `RootRelative str | `Component str -> str
    | `CurrentDir -> "."
    | `ParentDir -> ".."
  in
  let rec compare' =
    function
    | (hd1 :: tl1), (hd2 :: tl2) ->
      if hd1 = hd2 then
        compare' (tl1, tl2)
      else
        String.compare (fn_string hd1) (fn_string hd2)
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
  in
  compare' (fn_norm fn1, fn_norm fn2)


let to_unix hfn =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> hfn
  | "Win32" ->
    let rec to_unix_aux =
      function
      | `Root str :: _
      | `RootRelative str :: _ ->
        OASISUtils.failwithf
          (f_ "Cannot translate %S to unix filename, it contains a root \
               reference (%S).")
          hfn
          str
      | `Component str :: tl ->
        str :: (to_unix_aux tl)
      | `CurrentDir :: tl ->
        "." :: (to_unix_aux tl)
      | `ParentDir :: tl ->
        ".." :: (to_unix_aux tl)
      | [] ->
        []
    in
    OASISUnixPath.make (to_unix_aux (fn_norm hfn))
  | os_type ->
    OASISUtils.failwithf
      (f_ "Don't know the path format of os_type %S when translating host \
           filename. %S")
      os_type hfn


let add_extension fn ext =
  fn^"."^ext


module Map = OASISUtils.MapExt.Make (
  struct
    type t = Unix.host_filename

    let compare = compare
  end)
