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

(** Run test
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISMessage
open OASISTypes
open OASISExpr
open OASISGettext

let test lst pkg extra_args =

  let one_test (test_plugin, cs, test) =
    if var_choose test.test_run then
      begin
        let () = 
          info (f_ "Running test '%s'") cs.cs_name
        in
        let back_cwd = 
          match test.test_working_directory with 
            | Some dir -> 
                let cwd = 
                  Sys.getcwd ()
                in
                let chdir d =
                  info (f_ "Changing directory to '%s'") d;
                  Sys.chdir d
                in
                  chdir dir;
                  fun () -> chdir cwd

            | None -> 
                fun () -> ()
        in
          try 
            let failure_percent =
              test_plugin pkg (cs, test) extra_args 
            in
              back_cwd ();
              failure_percent
          with e ->
            begin
              back_cwd ();
              raise e
            end
      end
    else
      begin
        info (f_ "Skipping test '%s'") cs.cs_name;
        0.0
      end
  in
  let res =
    List.map
      one_test
      lst
  in
  let n = 
    float_of_int (List.length res)
  in
  let failure_percent =
    List.fold_left
      (fun r e -> r +. (e /. n))
      0.0
      res
  in
    (if failure_percent > 0.0 then
       warning 
     else
       info)
      (f_ "Tests had a %.2f%% failure rate")
      (100. *. failure_percent)

(* END EXPORT *)
