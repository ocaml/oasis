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


(* Work in progress, only for testing purpose
   Try to guess file to install depending on archive (.cma) and published
   interface (.cmi)
*)


let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()


    #use "topfind"
                        #require "unix"
                        #require "pcre"


let warning msg =
  prerr_endline msg


let error msg =
  prerr_endline msg;
  exit 1


let find_unit directory unit_name extensions =
  let unit_base_fn =
    [
      unit_name;
      (String.uncapitalize unit_name);
    ]
  in
  let unit_ext_fn =
    List.flatten
      (List.rev_map
         (fun unit_nm ->
            List.rev_map
              (fun ext -> unit_nm^"."^ext)
              extensions
         )
         unit_base_fn
      )
  in
  let unit_fn =
    List.rev_map
      (fun fn ->
         Filename.concat directory fn
      )
      unit_ext_fn
  in
  let () =
    prerr_endline (String.concat " -> " unit_fn)
  in
  let unit_exist_fn =
    List.filter
      Sys.file_exists
      unit_fn
  in
  match unit_exist_fn with
    | fn :: _ ->
      fn
    | [] ->
      raise Not_found


let cmx_of_cma cma =
  let dirname =
    Filename.dirname cma
  in
  let () =
    if not (Filename.check_suffix cma ".cma") then
      failwith (cma^" is not an OCaml library")
  in
  let unit_name =
    let reg =
      Pcre.regexp "Unit name: ([A-Z][A-Za-z0-9]*)"
    in
    fun line ->
      Pcre.get_substring (Pcre.exec ~rex:reg line) 1
  in
  let unit_list =
    ref []
  in
  let chn =
    Unix.open_process_in ("ocamlobjinfo "^cma)
  in
  let () =
    try
      while true do
        try
          unit_list := (unit_name (input_line chn)) :: !unit_list
        with Not_found ->
          ()
      done
    with End_of_file ->
      ()
  in
  let _ =
    Unix.close_process_in chn
  in
  let add_cmx acc unit_nm =
    try
      (find_unit dirname unit_nm ["cmx"]) :: acc
    with Not_found ->
      (
        warning ("Could not find .cmx file for unit '"^unit_nm^"'");
        acc
      )
  in
  List.fold_left add_cmx [] !unit_list


let install_lib ~directory ~archive ~interfaces =
  let mandatory_file fn =
    if not (Sys.file_exists fn) then
      error ("Could not find file '"^fn^"'");
    fn
  in
  let fn_cma =
    mandatory_file (Filename.concat directory (archive^".cma"))
  in
  let fn_a =
    mandatory_file (Filename.concat directory (archive^".a"))
  in
  let fn_lst_cmi =
    List.rev_map
      (fun interf ->
         try
           find_unit directory interf ["cmi"]
         with Not_found ->
           error ("Could not find interface file for unit '"^interf^"'")
      )
      interfaces
  in
  let fn_lst_mli =
    List.fold_left
      (fun acc interf ->
         try
           (find_unit directory interf ["mli"; "ml"; "mll"; "mly"]) :: acc
         with Not_found ->
           (
             warning ("Could not find interface source file for unit '"^
                 interf^"'");
             acc
           )
      )
      []
      interfaces
  in
  let fn_lst_stubs =
    (* TODO *)
    List.flatten
      (List.rev_map
         (fun archive ->
            let fn_stubs_a =
              (* FIXME msvc: .lib .dll ? *)
              mandatory_file
                (Filename.concat directory ("lib"^archive^"_stubs.a"))
            in
            let fn_stubs_so =
              mandatory_file
                (Filename.concat directory ("dll"^archive^"_stubs.so"))
            in
            [fn_stubs_a; fn_stubs_so]
         )
         []
      )
  in
  let fn_lst_native =
    let fn_cmxa =
      Filename.concat directory (archive^".cmxa")
    in
    if Sys.file_exists fn_cmxa then
      (
        let fn_cmx =
          cmx_of_cma fn_cma
        in
        fn_cmxa :: fn_cmx
      )
    else
      (
        []
      )
  in
  List.flatten
    [
      [fn_cma; fn_a];
      fn_lst_cmi;
      fn_lst_mli;
      fn_lst_native;
      fn_lst_stubs;
    ]


(* QUID: .o (.obj), .a (.lib), .so (.dll) *)


let () =
  print_endline
    ("To install: "^
       (String.concat ", "
          (install_lib ~directory:"." ~archive:"stlang"
             ~interfaces:["STLang"; "STLangTypes"])))

