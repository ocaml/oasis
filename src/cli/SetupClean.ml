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


(** Clean generated template files
    @author Sylvain Le Gall
*)


open CLISubCommand
open OASISGettext
open OASISFileTemplate
open OASISPlugin


let main ~ctxt (replace_sections, remove) oasis_fn pkg =
  BaseGenerate.restore ();
  if replace_sections then begin
    let ctxt, _ = BaseSetup.of_package ~oasis_fn ~setup_update:false OASISSetupUpdate.NoUpdate pkg in
    OASISFileTemplate.fold
      (fun tmpl () ->
         match tmpl.body with
           | Body _
           | BodyWithDigest _ ->
             begin
               let _chng: file_generate_change =
                 file_generate
                   ~ctxt:!BaseContext.default
                   ~remove
                   ~backup:false
                   {tmpl with body = Body []}
               in
               ()
             end
           | NoBody ->
             ())
      ctxt.files
      ()
  end


let () =
  CLISubCommand.register "setup-clean"
    (ns_ "Clean all template files from their content")
    CLIData.setup_clean_mkd
    (CLICommon.parse_oasis_fn
       (CLISubCommand.make_run
          (fun () ->
             let replace_sections = ref false in
             let remove = ref false in
             (["-replace-sections",
               Arg.Set replace_sections,
               s_ "Empty replace section in generated files (i.e. remove \
                   content between OASIS_START and OASIS_STOP).";
               "-remove",
               Arg.Set remove,
               s_ "Empty remove files which have unaltered header and footer."],
              CLISubCommand.default_anon),
             (fun () -> !remove || !replace_sections, !remove))
          main))
