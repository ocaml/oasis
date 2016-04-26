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

open OUnit2


let pluginloader = Conf.make_exec "pluginloader"

let datadir = FilePath.make_filename 
    ["src"; "ext"; "plugin-loader"; "test"; "data"]
let findlibdir = FilePath.concat datadir "findlib"


let setup_findlib test_ctxt =
  let dn = bracket_tmpdir test_ctxt in
  let builddir = FilePath.make_absolute (FileUtil.pwd ()) "_build" in
  let buildfindlibdir = FilePath.concat builddir findlibdir in
  let findlibdir = FilePath.make_absolute (FileUtil.pwd ()) findlibdir in
  let copy_to_dn fn_dir fn =
    let tgt = FilePath.reparent fn_dir dn fn in
    FileUtil.mkdir ~parent:true (FilePath.dirname tgt);
    logf test_ctxt `Info "Copy file '%s' to '%s'\n%!" fn tgt;
    FileUtil.cp [fn] tgt
  in
  (* Find all the .mlldir in findlibdir and copy .cma/.cmxs from
   * the _build dir and then all the META.
  *)
  logf test_ctxt `Info "Findlib dir: %s" findlibdir;
  FileUtil.find
    (FileUtil.Or
       (FileUtil.Has_extension "cma",
        FileUtil.Has_extension "cmxs"))
    buildfindlibdir
    (fun () fn ->
       copy_to_dn buildfindlibdir fn)
    ();
  FileUtil.find
    (FileUtil.Basename_is "META")
    findlibdir
    (fun () fn ->
       copy_to_dn findlibdir fn)
    ();
  dn


let assert_pluginloader test_ctxt dn args =
  let buf = Buffer.create 13 in
  let lst = ref [] in
  let ocamlpath =
    try
      FilePath.string_of_path
        ((FilePath.path_of_string (Sys.getenv "OCAMLPATH")) @ [dn])
    with Not_found ->
      dn
  in
  let env =
    Array.append
      [|"OCAMLPATH="^ocamlpath|]
      (Unix.environment ())
  in
  assert_command
    ~env
    ~ctxt:test_ctxt
    ~use_stderr:true
    ~foutput:(Stream.iter
        (function
          | '\n' ->
            lst := Buffer.contents buf :: !lst;
            Buffer.clear buf
          | c ->
            Buffer.add_char buf c))
    (pluginloader test_ctxt) args;

  List.rev (Buffer.contents buf :: !lst)


let () =
  run_test_tt_main
    ("PluginLoader" >:::
       [
         "list" >::
           (fun test_ctxt ->
              let dn = setup_findlib test_ctxt in
              let lst = assert_pluginloader test_ctxt dn [] in
              assert_equal
                ~printer:(String.concat ", ")
                ["plugin1: first plugin";
                 "plugin2: second plugin";
                 "plugin3: third plugin";
                 ""]
                lst);

         "load" >::
           (fun test_ctxt ->
              let dn = setup_findlib test_ctxt in
              let lst = assert_pluginloader test_ctxt dn ["-load"; "plugin1"] in
              assert_equal
                ~printer:(String.concat ", ")
                ["plugin_loaded: plugin1"; ""]
                lst)
       ])
