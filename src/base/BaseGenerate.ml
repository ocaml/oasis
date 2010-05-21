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

(** Generate package files
    @author Sylvain Le Gall
  *)

open Format
open OASISTypes
open OASISUtils
open BaseFileGenerate
open ODN
open OASISPlugin

let required_modules =
  [
    OASISData.oasissys_ml;
    BaseData.basesysenvironment_ml;
    (* TODO: is this module really required ? *)
    BaseData.basesys_ml;
  ]

(** Generate setup.ml and the rest of the build system 
  *)
let generate pkg dev setup_fn use_real_oasis_filename = 

  let pkg, setup_t_odn, other_actions, moduls =
    BaseSetup.odn_of_oasis pkg
  in

  let () = 
    (* Run extra plugin *)
    List.iter
      (fun nm -> Extra.find nm pkg)
      pkg.plugins
  in

  let moduls =
    let moduls =
      required_modules @ moduls
    in
    let (rmoduls, _) =
      List.fold_left
        (fun ((moduls, moduls_seen) as acc) modul ->
           if SetString.mem modul moduls_seen then
             acc
           else
             (modul :: moduls, SetString.add modul moduls_seen))
        ([], SetString.empty)
        moduls
    in
      List.rev rmoduls
  in

  let setup_fun =
    fprintf str_formatter
      "@[<v>open OASISTypes;;@,@[<hv2>let setup () =@ %a@,@];;"
      (pp_odn ~opened_modules:["OASISTypes"]) 
      (if dev then
         APP ("BaseDev.update_and_run", 
              [], 
              [BaseDev.odn_of_t 
                 (BaseDev.create 
                    use_real_oasis_filename 
                    setup_fn)])
       else
         APP ("BaseSetup.setup", 
              [], 
              [setup_t_odn]));
    flush_str_formatter ()
  in

    (* Generate setup.ml *)
    mlfile_generate
      setup_fn
      (Split
         (
           (* Header *)
           [],
           (* Body *)
           (List.flatten 
              [
                moduls;
                [setup_fun]
              ]),
           (* Footer *)
           ["let () = setup ();;"]
         )
      );

    (* Generate other files *)
    List.iter
      (fun act -> act ())
      other_actions

