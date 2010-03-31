(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** {1 Checking for particular features} 
  *)

open BaseEnv
open OASISUtils
open OASISGettext

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  var_redefine
    prg 
    (lazy 
       (let alternate = 
          List.fold_left 
            (fun res e ->
               match res with 
                 | Some _ -> 
                     res
                 | None ->
                     try
                       Some (BaseFileUtil.which e)
                     with Not_found ->
                       None)
            None
            prg_lst
        in
          match alternate with
            | Some prg -> prg
            | None -> raise Not_found))

(** Check the presence of a particular program.
  *)
let prog prg =
  prog_best prg [prg]

(** Check the presence of a program or its native version
  *)
let prog_opt prg = 
  prog_best prg [prg^".opt"; prg]

let ocamlfind = 
  prog "ocamlfind"

(** Check version, following Sys.ocaml_version convention
  *)
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
      (lazy
         (let version_str =
            match fversion () with 
              | "[Distributed with OCaml]" ->
                  begin
                    try 
                      (var_get "ocaml_version")
                    with Not_found ->
                      OASISMessage.warning 
                        "Variable ocaml_version not defined, fallback to default";
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
              failwithf3
                (f_ "Cannot satisfy version constraint on %s: %s (version: %s)")
                var_prefix
                (OASISVersion.string_of_comparator cmp)
                version_str))
      ()

(** Get findlib package version 
  *)
let package_version pkg =
  BaseExec.run_read_one_line 
    (ocamlfind ())
    ["query"; "-format"; "%v"; pkg]

(** Check for findlib package
  *)
let package ?version_comparator pkg () =
  let var =
    OASISUtils.varname_concat 
      "pkg_" 
      (OASISUtils.varname_of_string pkg)
  in
  let findlib_dir pkg = 
    let dir = 
      BaseExec.run_read_one_line
        (ocamlfind ())
        ["query"; "-format"; "%d"; pkg]
    in
      if Sys.file_exists dir && Sys.is_directory dir then
        dir
      else
        failwithf2
          (f_ "When looking for findlib package %s, \
               directory %s return doesn't exist")
          pkg dir
  in
  let vl =
    var_redefine
      var
      (lazy (findlib_dir pkg))
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
