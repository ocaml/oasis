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
open OASISTypes
open OASISGettext


let run cmd args extra_args =
  OASISExec.run ~ctxt:!BaseContext.default ~quote:false
    (var_expand cmd)
    (List.map
       var_expand
       (args @ (Array.to_list extra_args)))


let hook ?(failsafe=false) cstm f e =
  let optional_command lst =
    let printer =
      function
        | Some (cmd, args) -> String.concat " " (cmd :: args)
        | None -> s_ "No command"
    in
    match
      var_choose
        ~name:(s_ "Pre/Post Command")
        ~printer
        lst with
      | Some (cmd, args) ->
        begin
          try
            run cmd args [||]
          with e when failsafe ->
            warning
              (f_ "Command '%s' fail with error: %s")
              (String.concat " " (cmd :: args))
              (match e with
                | Failure msg -> msg
                | e -> Printexc.to_string e)
        end
      | None ->
        ()
  in
  let res =
    optional_command cstm.pre_command;
    f e
  in
  optional_command cstm.post_command;
  res
