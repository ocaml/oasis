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


open BaseEnv
open BaseMessage
open OASISUtils
open OASISGettext


let prog_best prg prg_lst =
  var_redefine
    prg
    (fun () ->
       let alternate =
         List.fold_left
           (fun res e ->
              match res with
                | Some _ ->
                  res
                | None ->
                  try
                    Some (OASISFileUtil.which ~ctxt:!BaseContext.default e)
                  with Not_found ->
                    None)
           None
           prg_lst
       in
       match alternate with
         | Some prg -> prg
         | None -> raise Not_found)


let prog prg =
  prog_best prg [prg]


let prog_opt prg =
  prog_best prg [prg^".opt"; prg]


let ocamlfind =
  prog "ocamlfind"


let version
    var_prefix
    cmp
    fversion
    () =
  (* Really compare version provided *)
  let var =
    var_prefix^"_version_"^(OASISVersion.varname_of_comparator cmp)
  in
  var_redefine
    ~hide:true
    var
    (fun () ->
       let version_str =
         match fversion () with
           | "[Distributed with OCaml]" ->
             begin
               try
                 (var_get "ocaml_version")
               with Not_found ->
                 warning
                   (f_ "Variable ocaml_version not defined, fallback \
                        to default");
                 Sys.ocaml_version
             end
           | res ->
             res
       in
       let version =
         OASISVersion.version_of_string version_str
       in
       if OASISVersion.comparator_apply version cmp then
         version_str
       else
         failwithf
           (f_ "Cannot satisfy version constraint on %s: %s (version: %s)")
           var_prefix
           (OASISVersion.string_of_comparator cmp)
           version_str)
    ()


let package_version pkg =
  OASISExec.run_read_one_line ~ctxt:!BaseContext.default
    (ocamlfind ())
    ["query"; "-format"; "%v"; pkg]


let package ?version_comparator pkg () =
  let var =
    OASISUtils.varname_concat
      "pkg_"
      (OASISUtils.varname_of_string pkg)
  in
  let findlib_dir pkg =
    let dir =
      OASISExec.run_read_one_line ~ctxt:!BaseContext.default
        (ocamlfind ())
        ["query"; "-format"; "%d"; pkg]
    in
    if Sys.file_exists dir && Sys.is_directory dir then
      dir
    else
      failwithf
        (f_ "When looking for findlib package %s, \
             directory %s return doesn't exist")
        pkg dir
  in
  let vl =
    var_redefine
      var
      (fun () -> findlib_dir pkg)
      ()
  in
  (
    match version_comparator with
      | Some ver_cmp ->
        ignore
          (version
             var
             ver_cmp
             (fun _ -> package_version pkg)
             ())
      | None ->
        ()
  );
  vl
