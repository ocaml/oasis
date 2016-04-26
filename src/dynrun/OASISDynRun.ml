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


(* Dynamic runtime for building setup.ml.

   We use this to reduce setup.ml replaceable section to a few lines
   everything else is dynamically generated.
*)


let () = OASISBuiltinPlugins.init ()


open OASISTypes
open BaseSetup


let setup_t =
  let pkg =
    OASISParse.from_file ~ctxt:!BaseContext.default OASISParse.default_oasis_fn
  in
  let _, setup_t = BaseSetup.of_package ~setup_update:false OASISSetupUpdate.Dynamic pkg in
  setup_t


(* Re-export BaseSetup.setup so one can modify setup_t before passing it
   to setup(). *)
module BaseSetup = struct
  include BaseSetup

  let setup setup_t =
    let tmp_setup_fn =
      Filename.temp_file (setup_t.package.name^"-setup") ".ml" in
    let restored = ref false in
    let cleanup () =
      if not !restored then begin
        restored := true;
        BaseGenerate.restore ();
        if Sys.file_exists tmp_setup_fn then
          Sys.remove tmp_setup_fn
      end
    in
    try
      let _lst: 'a list =
        BaseGenerate.generate
          ~backup:true
          ~setup_fn:tmp_setup_fn
          ~restore:true
          OASISSetupUpdate.NoUpdate
          (OASISParse.from_file
             ~ctxt:!BaseContext.default OASISParse.default_oasis_fn)
      in
      at_exit cleanup;
      let setup_t =
        (* Override distclean, because it remove setup.log and we need it for
         * BaseGenerate.restore
        *)
        {setup_t with distclean = setup_t.distclean @ [fun _ _ -> cleanup ()]}
      in
      BaseSetup.setup setup_t;
      cleanup ()
    with e ->
      cleanup ();
      raise e
end


let setup () = BaseSetup.setup setup_t
