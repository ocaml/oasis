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

TYPE_CONV_PATH "BaseDev"

open OASISGettext
open BaseMessage

type t =
    {
      oasis_cmd:  string;
    } with odn

let update_and_run t =
  (* Command line to run setup-dev *)
  let oasis_args =
    "setup-dev" :: "-run" ::
    Sys.executable_name ::
    (Array.to_list Sys.argv)
  in

  let exit_on_child_error =
    function
      | 0 -> ()
      | 2 ->
          (* Bad CLI arguments *)
          error
            (f_ "The command '%s %s' exit with code 2. It often means that we \
                 don't use the right command-line arguments, rerun \
                 'oasis setup-dev'.")
            t.oasis_cmd
            (String.concat " " oasis_args)

      | 127 ->
          (* Cannot find OASIS *)
          error
            (f_ "Cannot find executable '%s', check where 'oasis' is located \
                 and rerun 'oasis setup-dev'")
            t.oasis_cmd

      | i ->
          exit i
  in

  let () =
    (* Run OASIS to generate a temporary setup.ml
     *)
    BaseExec.run
      ~f_exit_code:exit_on_child_error
      t.oasis_cmd
      oasis_args
  in

    ()

(* END EXPORT *)

open OASISFileTemplate
open OASISPlugin

let make ?oasis_exec ctxt pkg =

  let setup_tmpl =
    BaseSetup.find ctxt
  in

  let dev =
    {
      oasis_cmd =
        (match oasis_exec with
           | Some fn -> fn
           | None    -> "oasis");
    }
  in

  let dev_str =
    Format.fprintf Format.str_formatter
      "@[<hv2>let dev_t =@ %a;;@]"
      (fun fmt -> ODN.pp_odn fmt)
      (odn_of_t dev);
    Format.flush_str_formatter ()
  in

  let setup_tmpl =
    let body_lst =
      match setup_tmpl.body with
        | NoBody -> []
        | Body lst
        | BodyWithDigest (_, lst) -> lst
    in
      {setup_tmpl with
           body =
             Body
               (body_lst
                @
                [
                  "";
                  dev_str;
                  "";
                  "let setup () = BaseDev.update_and_run dev_t;;";
                  "";
                ])}
  in

  let ctxt =
    {ctxt with
         files =
           OASISFileTemplate.replace
             setup_tmpl
             ctxt.files}
  in

    ctxt, dev

