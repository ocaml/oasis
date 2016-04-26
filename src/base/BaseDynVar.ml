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


open OASISTypes
open OASISGettext
open BaseEnv
open BaseBuilt


let init pkg =
  (* TODO: disambiguate exec vs other variable by adding exec_VARNAME. *)
  (* TODO: provide compile option for library libary_byte_args_VARNAME... *)
  List.iter
    (function
      | Executable (cs, bs, exec) ->
        if var_choose bs.bs_build then
          var_ignore
            (var_redefine
               (* We don't save this variable *)
               ~dump:false
               ~short_desc:(fun () ->
                 Printf.sprintf
                   (f_ "Filename of executable '%s'")
                   cs.cs_name)
               (OASISUtils.varname_of_string cs.cs_name)
               (fun () ->
                  let fn_opt =
                    fold
                      BExec cs.cs_name
                      (fun _ fn -> Some fn)
                      None
                  in
                  match fn_opt with
                    | Some fn -> fn
                    | None ->
                      raise
                        (PropList.Not_set
                           (cs.cs_name,
                            Some (Printf.sprintf
                                (f_ "Executable '%s' not yet built.")
                                cs.cs_name)))))

      | Library _ | Object _ | Flag _ | Test _ | SrcRepo _ | Doc _ ->
        ())
    pkg.sections
