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


type unix_filename = string
type unix_dirname = string


type host_filename = string
type host_dirname = string


let current_dir_name = "."


let parent_dir_name = ".."


let is_current_dir fn =
  fn = current_dir_name || fn = ""


let concat f1 f2 =
  if is_current_dir f1 then
    f2
  else
    let f1' =
      try OASISString.strip_ends_with ~what:"/" f1 with Not_found -> f1
    in
    f1'^"/"^f2


let make =
  function
    | hd :: tl ->
      List.fold_left
        (fun f p -> concat f p)
        hd
        tl
    | [] ->
      invalid_arg "OASISUnixPath.make"


let dirname f =
  try
    String.sub f 0 (String.rindex f '/')
  with Not_found ->
    current_dir_name


let basename f =
  try
    let pos_start =
      (String.rindex f '/') + 1
    in
    String.sub f pos_start ((String.length f) - pos_start)
  with Not_found ->
    f


let chop_extension f =
  try
    let last_dot =
      String.rindex f '.'
    in
    let sub =
      String.sub f 0 last_dot
    in
    try
      let last_slash =
        String.rindex f '/'
      in
      if last_slash < last_dot then
        sub
      else
        f
    with Not_found ->
      sub

  with Not_found ->
    f


let capitalize_file f =
  let dir = dirname f in
  let base = basename f in
  concat dir (String.capitalize base)


let uncapitalize_file f =
  let dir = dirname f in
  let base = basename f in
  concat dir (String.uncapitalize base)


(* END EXPORT *)


let check_extension fn ext =
  OASISString.ends_with ~what:("."^ext) fn


let add_extension fn ext =
  fn^"."^ext


let replace_extension fn ext =
  add_extension (Filename.chop_extension fn) ext


open OASISPath_intern
open OASISUtils


let filename_of_list lst =
  let buf = Buffer.create 34 in
  List.iter
    (function
      | `Root _ | `RootRelative _ -> Buffer.add_char buf '/'

      | `Component str ->
        if Buffer.length buf > 0 then
          Buffer.add_char buf '/';
        Buffer.add_string buf str

      | `CurrentDir ->
        if Buffer.length buf > 0 then
          begin
            Buffer.add_char buf '/';
            Buffer.add_string buf current_dir_name
          end

      | `ParentDir ->
        if Buffer.length buf > 0 then
          Buffer.add_char buf '/';
        Buffer.add_string buf parent_dir_name)
    lst;
  Buffer.contents buf


let fn_norm fn = fn_reduce [] (fn_reader ~os_type:"Unix" fn)


let reduce fn = filename_of_list (fn_norm fn)


let make_relative fn_root fn =
  (* Basic analysis of fn_root and fn. *)
  let lst_root = fn_norm fn_root in
  let lst = fn_norm fn in
  let existing_component =
    List.fold_left
      (fun acc ->
         function
           | `Component str -> SetString.add str acc
           | _ -> acc)
      SetString.empty
      (List.rev_append lst_root lst)
  in

  (* Create a fake absolute path that will can be used to make
   * fn_root and fn absolute.
   * It covers pathological case like "../../../../" as a filename.
  *)
  let fake_root =
    let abs_len =
      max (List.length lst_root)  (List.length lst) + 1
    in
    let idx = ref 0 in
    let rec cpt_uniq i =
      let cpt = Printf.sprintf "c%d" !idx in
      incr idx;
      if SetString.mem cpt existing_component then
        cpt_uniq i
      else
        `Component cpt
    in
    filename_of_list (`Root "" :: Array.to_list (Array.init abs_len cpt_uniq))
  in


  (* Use the created fake root to make fn and fn_root absolute. *)
  let make_absolute lst fn =
    match lst with
      | `Root _ :: _ -> lst
      | _ -> fn_norm (concat fake_root fn)
  in

  let abs_lst_root = make_absolute lst_root fn_root in
  let abs_lst = make_absolute lst fn in

  (* Compute relative path. *)
  let rec make_relative' =
    function
      | hd_root :: tl_root, hd :: tl when hd_root = hd ->
        make_relative' (tl_root, tl)
      | lst_root, lst ->
        let back_to_base = List.rev_map (fun _ -> `ParentDir) lst_root in
        back_to_base @ lst
  in
  let res =
    filename_of_list (fn_reduce [] (make_relative' (abs_lst_root, abs_lst)))
  in
  (* Check result. *)
  List.iter
    (function
      | `Component str ->
        if not (SetString.mem str existing_component) then
          OASISUtils.failwithf
            "When trying to 'make_relative %S %S' create a \
             non-existent path component %S"
            fn_root fn str
      | _ ->
        ())
    (fn_norm res);
  res


let is_current fn =
  if fn = current_dir_name || fn = "" then
    true
  else
    (fn_norm fn) = [`CurrentDir]


module Set = SetExt.Make(
  struct
    type t = unix_filename

    let compare t1 t2 = String.compare (reduce t1) (reduce t2)
  end)
