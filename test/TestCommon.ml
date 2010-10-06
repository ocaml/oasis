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

(** Common utilities for testing
    @author Sylvain Le Gall
  *)

IFDEF HAS_GETTEXT THEN
module Gettext =
  Gettext.Program
    (struct
       let textdomain   = "oasis"
       let codeset      = None
       let dir          = None
       let dependencies = Gettext.init @ OASISGettext.init
     end)
    (GettextStub.Native)
ELSE
module Gettext =
struct 
  let init = [], ""
end
ENDIF

open OUnit

module MapString = Map.Make(String)
module SetString = Set.Make(String)

let dbug = ref false
let long = ref false
let has_ocamlopt = ref false 
let oasis_exec = ref None
let oasis_args = ref []

let oasis () = 
  match !oasis_exec with 
    | None -> 
        failwith "You must define oasis executable with -oasis-exec"
    | Some e ->
        e

let oasis_ctxt = 
  ref OASISContext.quiet

let set_verbose b = 
  dbug := b;
  if b then 
    oasis_ctxt := !OASISContext.default

let test_args = 
  let gettext_args, _ =
    Gettext.init 
  in
    [
      "-long",
      Arg.Set long,
      " Run long tests";

      "-has-ocamlopt",
      Arg.String (fun s -> has_ocamlopt := bool_of_string s),
      "bool Can use ocamlopt for tests";

      "-oasis-exec",
      Arg.String (fun s -> oasis_exec := Some s),
      "fn Define oasis executable";

      "-oasis-args",
      Arg.Rest (fun str -> oasis_args := !oasis_args @ [str]),
      "args* Define oasis arguments";
    ] @ gettext_args @ (BaseContext.args ())

let in_data fn =
  Filename.concat "data" fn
;;

(* Create a temporary dir *)
let temp_dir () =
  let res = 
    Filename.temp_file "oasis-" ".dir"
  in
    FileUtil.rm [res];
    FileUtil.mkdir res;
    at_exit 
      (fun () -> 
         FileUtil.rm ~recurse:true [res]);
    res

(* Assert checking that command run well *)
let assert_command ?exit_code ?output ?extra_env cmd args  =
  let foutput = 
    match output with 
      | Some str ->
          let foutput strm = 
            let buff = Buffer.create 13 in
              Stream.iter (Buffer.add_char buff) strm;
              assert_equal 
                ~msg:(Printf.sprintf "'%s' command output" 
                        (String.concat " " (cmd :: args)))
                ~printer:(Printf.sprintf "%S")
                str
                (Buffer.contents buff)
          in
            Some foutput

      | None -> 
          None
  in
  let env = 
    let min_env = 
      try 
        ["PATH="^(Sys.getenv "PATH")]
      with Not_found ->
        []
    in
    let extra_env = 
      match extra_env with 
        | Some lst ->
            List.map (fun (k,v) -> k^"="^v) lst

        | None ->
            []
    in
      Some (Array.of_list (min_env @ extra_env))
  in
    
    assert_command 
      ?foutput ?env ?exit_code ~use_stderr:true ~verbose:!dbug 
      cmd args

let assert_oasis_cli ?exit_code ?output ?extra_env args  =
  assert_command ?exit_code ?output ?extra_env 
    (oasis ()) (!oasis_args @ args)
