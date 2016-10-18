(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


(** Test for OASISFileSystem.
   @author Sylvain Le Gall
*)

open OUnit2
open OASISFileSystem

let tests =
  "OASISFileSystem" >:::
  [
    "defer_close" >::
    (fun _ ->
       let tester msg =
         let clsr =
           object
             val count = ref 0
             method close = incr count
             method get_count = !count
           end
         in
         clsr,
         fun () ->
           assert_equal
             ~msg:(msg ^ ": number of time #close has been called.")
             ~printer:string_of_int
             1
             clsr#get_count
       in
       let clsr, assrt = tester "normal" in
       let () =
         defer_close clsr ignore;
         assrt ()
       in
       let clsr, assrt = tester "raise exception" in
       let exc = Failure "foobar" in
       let () =
         try
           defer_close clsr (fun _ -> if true then raise exc; ());
           assert_failure "raise exception should have raised an exception"
         with e when e = exc ->
           assrt ()
       in
       ());

    "host_fs" >::
    (fun test_ctxt ->
       let tmpdir = bracket_tmpdir test_ctxt in
       let hfs = new host_fs tmpdir in
       let fn = of_unix_filename "foobar" in
       let content = "abcd" in

       let assert_present fn =
         assert_bool
           (Printf.sprintf
              "File %S should exist."
              (hfs#string_of_filename fn))
           (hfs#file_exists fn)
       in

       let assert_absent fn =
         assert_bool
           (Printf.sprintf
              "File %S should not exist."
              (hfs#string_of_filename fn))
           (not (hfs#file_exists fn))
       in

       let buf = Buffer.create (String.length content) in

       assert_absent fn;
       defer_close (hfs#open_out fn) ignore;
       assert_present fn;
       defer_close
         (hfs#open_out fn)
         (fun wrtr ->
            Buffer.clear buf;
            Buffer.add_string buf content;
            wrtr#output buf);
       Buffer.clear buf;
       defer_close (hfs#open_in fn) (read_all buf);
       assert_equal
         ~msg:"File content"
         ~printer:(Printf.sprintf "%S")
         content
         (Buffer.contents buf);
       hfs#remove fn;
       assert_absent fn;
       ());

    "stream_of_reader" >::
    (fun test_ctxt ->
       let tmpdir = bracket_tmpdir test_ctxt in
       let fn = of_unix_filename "test.txt" in
       let hfs = new host_fs tmpdir in
       let buf = Buffer.create 13 in

       let test_one len =
         let str =
           Buffer.clear buf;
           for i = 0 to len - 1 do
             Buffer.add_char buf (Char.chr ((Char.code 'a') + (i mod 26)))
           done;
           defer_close
             (hfs#open_out fn)
             (fun wrtr -> wrtr#output buf);
           Buffer.contents buf
         in
         defer_close
           (hfs#open_in fn)
           (fun rdr ->
              let strm = stream_of_reader rdr in
              Buffer.clear buf;
              Stream.iter (Buffer.add_char buf) strm);
         assert_equal ~printer:(Printf.sprintf "%S") str (Buffer.contents buf)
       in
       test_one 15;
       test_one 15000);
  ]
