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

(** Entry points for setup.ml in dev mode
  *)

TYPE_CONV_PATH "BaseDev"

open OASISGettext
open OASISMessage

type t = 
    {
      oasis_cmd:  string;
      self_fn:    string;
    } with odn

let update_and_run t = 
  let dev_fn = 
    "setup-dev.ml"
  in

  (* Command to run after creation of dev_fn *)
  let bootstrap_ocaml = 
    Sys.executable_name
  in
  let bootstrap_args =
    Array.to_list
      (Array.map
         (fun a ->
            if a = t.self_fn then
              dev_fn
            else
              a)
         Sys.argv)
  in

  let safe_exit () = 
    if Sys.file_exists dev_fn then
      Sys.remove dev_fn;

    (* Restore backup files *)
    BaseExec.run 
      ~f_exit_code:(function
                      | 0 -> () 
                      | n -> error ~exit:false
                               (f_ "'%s setup-clean exit with code %d")
                               t.oasis_cmd
                               n)
      t.oasis_cmd 
      ["-quiet"; "setup-clean"];

  in

  let exit_on_child_error = 
    function
      | 0 -> ()
      | i -> exit i
  in

    at_exit safe_exit;

    if Sys.file_exists dev_fn then
      begin
        OASISMessage.error
          (f_ "File %s already exists, cannot generate it for \
               dev-mode. Please remove it first.")
          dev_fn;
      end
    else
      begin
        try 

          (* Run OASIS to generate a temporary setup.ml
           *)
          BaseExec.run 
            ~f_exit_code:exit_on_child_error
            t.oasis_cmd 
            ["-quiet"; "setup"; "-setup-fn"; dev_fn; "-backup"];

          (* Run own command line by replacing setup.ml by 
           * setup-dev.ml
           *)
          BaseExec.run
            ~f_exit_code:exit_on_child_error
            bootstrap_ocaml
            bootstrap_args;

        with e ->
          error "%s" (string_of_exception e)
      end

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
           | None    -> "OASIS");

      self_fn = setup_tmpl.fn;
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

