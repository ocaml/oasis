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


let fn_reader ?(os_type=Sys.os_type) fn =
  let fn_part_of_string =
    function
      | "."  -> `CurrentDir
      | ".." -> `ParentDir
      | str  -> `Component str
  in
  if os_type = "Unix" || os_type = "Cygwin" then
    begin
      let sep = '/' in
      let fbeg, fn' =
        try
          (fun lst -> (`Root "") :: lst),
          OASISString.strip_starts_with ~what:(String.make 1 sep) fn
        with Not_found ->
          (fun lst -> lst), fn
      in
      fbeg (List.map fn_part_of_string (OASISString.nsplit fn sep))
    end
  else if os_type = "Win32" then
    begin
      let lst =
        List.map fn_part_of_string
          (OASISString.nsplitf fn (fun c -> c = '\\' || c = '/'))
      in
      match lst with
        | `Component str :: tl ->
          begin
            try
              let drive_letter, rmng = OASISString.split str ':' in
              if rmng = "" then
                (`Root drive_letter) :: tl
              else
                (`RootRelative drive_letter) :: (`Component rmng) :: tl
            with Not_found ->
              lst
          end
        | lst ->
          lst
    end
  else
    invalid_arg "OASISHostPath.dir_reader"


let rec fn_reduce acc =
  function
    | ((`Root _) | (`RootRelative _)) as hd :: tl ->
      fn_reduce [hd] tl
    | (`CurrentDir | `Component "") :: tl ->
      fn_reduce acc tl
    | `Component _ :: `ParentDir :: tl ->
      fn_reduce [] (List.rev_append acc tl)
    | (`Component _ | `ParentDir) as hd :: tl ->
      fn_reduce (hd :: acc) tl
    | [] ->
      if acc = [] then
        [`CurrentDir]
      else
        List.rev acc
