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

(** Run full OASIS use cases
    @author Sylvain Le Gall
  *)

open TestCommon;;
open FileUtil;;
open OUnit;;

type filename = FilePath.filename;;

let compare_filename =
  if Sys.os_type = "Win32" then
    (* Win32 FS is case insensitive *)
    (fun a b ->
       String.compare 
         (String.lowercase a) 
         (String.lowercase b))
  else
    String.compare

module SetFileDigest = 
  Set.Make
    (struct
       type t = filename * Digest.t

       let compare (f1,d1) (f2, d2) =
         match compare_filename f1 f2 with
           | 0 ->
               String.compare d1 d2
           | n ->
               n
     end)
;;

module SetFile =
  Set.Make
    (struct
       type t = filename
       let compare = compare_filename
     end)
;;

type location_t = 
    {
      build_dir:     filename;
      ocaml_lib_dir: filename;
      bin_dir:       filename;
      lib_dir:       filename;
      data_dir:      filename;
      doc_dir:       filename;
      html_dir:      filename;
    }
;;

let assert_equal_ext ?cmp ?printer ?diff ?msg exp real =
  try 
    assert_equal ?cmp ?printer ?msg exp real 
  with (Failure txt) as e ->
    (match diff with 
       | Some f ->
           failwith 
             (txt ^ "\ndiff: " ^(f exp real))
       | None ->
           raise e)
;;

module OUnitSet (S:Set.S) =
struct
  let assert_equal ?msg ?printer exp real =
    match printer with 
      | Some printer ->
          assert_equal_ext 
            ?msg
            ~cmp:S.equal
            ~printer:(fun st ->
                        String.concat ", "
                          (List.rev_map printer
                             (S.elements st)))
            ~diff:(fun exp real ->
                     let added = 
                       S.diff real exp
                     in
                     let removed =
                       S.diff exp real
                     in
                       String.concat ", " 
                         (List.rev_append 
                            (List.rev_map 
                               (fun e -> "+"^(printer e)) 
                               (S.elements added))
                            (List.rev_map 
                               (fun e -> "-"^(printer e)) 
                               (S.elements removed))))
            exp 
            real

      | None ->
          assert_equal
            ?msg
            exp
            real
end
;;

module OUnitSetFileDigest = OUnitSet(SetFileDigest);;
module OUnitSetFile = OUnitSet(SetFile);;

let tests ctxt =

  (* Create a temporary dir *)
  let temp_dir pref suff =
    let res = 
      Filename.temp_file pref suff
    in
      rm [res];
      mkdir res;
      res
  in

  let set_file_of_file_digest st =
    SetFileDigest.fold
      (fun (fn, _) st ->
         SetFile.add fn st)
      st
      SetFile.empty
  in

  (* Assert with setup.ml *)
  let assert_run_setup ?exit_code ?extra_env args =
    assert_command ?exit_code ?extra_env ctxt "ocaml" ("setup.ml" :: args)
  in

  (* Files always generated *)
  let oasis_std_files = 
    [
      "setup.ml"; 
    ]
  in

  (* Files generated when ocamlbuild buildsys is used *)
  let oasis_ocamlbuild_files =
    [
      "myocamlbuild.ml"; 
      "_tags";
    ]
  in

  (* Set all files location into build_dir + lib_dir *)
  let in_library files loc acc =
    List.fold_left
      (fun acc fn ->
         (FilePath.concat loc.lib_dir fn)
         ::
         acc)
      acc
      files
  in

  (* Set all files location into build_dir + ocaml_lib_dir + library *) 
  let in_ocaml_library lib files loc acc = 
    List.fold_left
      (fun acc fn ->
         (FilePath.make_filename [loc.ocaml_lib_dir; lib; fn])
         ::
         acc)
      acc
      files
  in

  (* Set all files location into html_dir + library *)
  let in_html lib files loc acc =
    List.fold_left
      (fun acc fn ->
         (FilePath.make_filename [loc.html_dir; lib; fn])
         ::
         acc)
      acc
      files
  in

  let api_ref_html lib moduls =
    in_html lib
      (List.rev_append
         [
           "index.html"; 
           "index_attributes.html"; 
           "index_class_types.html"; 
           "index_classes.html"; 
           "index_exceptions.html"; 
           "index_methods.html"; 
           "index_module_types.html"; 
           "index_modules.html"; 
           "index_types.html"; 
           "index_values.html"; 
           "style.css"; 
         ]
         (List.flatten
            (List.rev_map 
               (fun modul ->
                  ["type_"^modul^".html";
                   modul^".html"])
               moduls)))
  in

  (* Set all files location into buid_dir + data *)
  let in_data_dir files loc acc =
    List.fold_left
      (fun acc fn ->
         (FilePath.concat loc.data_dir fn) :: acc)
      acc
      files
  in

  (* Set all file location into bin_dir *)
  let in_bin files loc acc =
    List.fold_left
      (fun acc fn ->
         (FilePath.concat loc.bin_dir fn) :: acc)
      acc
      files
  in

  (* Run target conditionally *)
  let conditional cond f loc acc =
    if cond then
      f loc acc
    else
      acc
  in

  (* Try to run an installed executable *)
  let try_installed_exec ?exit_code cmd args loc =

    (* Libraries located inside the test
     * directory
     *)
    let local_lib_paths = 
      find
        Is_dir
        loc.lib_dir 
        (fun acc fn -> fn :: acc)
        (find 
           Is_dir
           loc.ocaml_lib_dir
           (fun acc fn -> fn :: acc)
           [])
    in

    let env_paths =
      try
        FilePath.path_of_string (Unix.getenv "PATH")
      with Not_found ->
        []
    in

    let paths, extra_env = 
      if Sys.os_type = "Win32" then
        begin
          (loc.bin_dir :: (local_lib_paths @ env_paths)),
          []
        end
      else
        begin
          let paths = 
            loc.bin_dir :: env_paths
          in
          let ld_library_paths = 
            local_lib_paths @
            (try
               FilePath.path_of_string (Unix.getenv "LD_LIBRARY_PATH")
             with Not_found ->
               [])
          in
            paths,
            [
              "LD_LIBRARY_PATH", ld_library_paths;
            ]
        end
    in

    let test () = 
      let real_cmd = 
        try 
          which ~path:paths cmd
        with Not_found ->
          assert_failure
            (Printf.sprintf 
               "Command '%s' cannot be found in %s"
               cmd
               (String.concat ";" paths))
      in
        assert_command 
          ?exit_code
          ~extra_env:(("OCAMLPATH", loc.ocaml_lib_dir)
                      ::
                      (List.map
                         (fun (v, lst) ->
                            v, FilePath.string_of_path lst)
                         (("PATH", paths) :: extra_env)))
          ctxt
          real_cmd
          args
    in
      fun acc -> test :: acc
  in

  let try_installed_library pkg modules loc acc = 
    let test () = 
      (* Create a file that contains every modules *)
      let srcdir = 
        let res =
          FilePath.concat loc.build_dir ("src-"^pkg)
        in
          mkdir res;
          res
      in

        try 
          let pkg_as_module =
            let res =
              Buffer.create (String.length pkg)
            in
              String.iter
                (function 
                   | '-' -> Buffer.add_char res '_'
                   | c -> Buffer.add_char res c)
                pkg;
              Buffer.contents res
          in
          let fn = 
            FilePath.concat srcdir ("test_"^pkg_as_module^".ml")
          in
          let extra_env = 
            ["OCAMLPATH", loc.ocaml_lib_dir]
          in
          let assert_compile cmd args =
            assert_command 
              ~extra_env 
              ctxt
              "ocamlfind" 
              (cmd :: "-package" :: pkg :: args )
          in
          let () = 
            (* Fill the file with open statement *)
            let chn_out =
              open_out fn
            in
              List.iter
                (Printf.fprintf chn_out "open %s;;\n")
                modules;
              close_out chn_out
          in
            (* Library + bytecode compilation *)
            assert_compile 
              "ocamlc" 
              ["-a"; 
               "-o"; FilePath.replace_extension fn "cma"; 
               fn];

            (* Program + bytecode compilation *)
            assert_compile 
              "ocamlc" 
              ["-o"; FilePath.replace_extension fn "byte"; 
               fn];

            if ctxt.has_ocamlopt then
              begin
                (* Library + native compilation *)
                assert_compile 
                  "ocamlopt" 
                  ["-a"; 
                   "-o"; FilePath.replace_extension fn "cmxa";
                   fn];

                (* Program + native compilation *)
                assert_compile 
                  "ocamlopt" 
                  ["-o"; FilePath.replace_extension fn "native"; 
                   fn];
              end;

            rm ~recurse:true [srcdir];

        with e ->
          (
            rm ~recurse:true [srcdir];
            raise e
          )
    in
      test :: acc
  in

  (* List all files in directory *)
  let all_files dir =
    find 
      Is_file
      dir
      (fun st fn -> SetFile.add fn st)
      SetFile.empty
  in

  (* List all files in current working dir *)
  let all_files_cwd () = 
    all_files (pwd ())
  in

  (* Create a set of file/digest of the current directory *)
  let all_file_digests () = 
    SetFile.fold
      (fun fn st ->
         SetFileDigest.add (fn, Digest.file fn) st)
      (all_files_cwd ())
      SetFileDigest.empty
  in

  (* Print a short version of the filename *)
  let fn_printer ?(root = pwd ()) fn =
    Printf.sprintf "'%s'" (FilePath.make_relative root fn)
  in

  let mkloc build_dir =
    (* Create a directory in build_dir and return its name *)
    let mkdir_return fn_parts =
      let fn = 
        FilePath.make_filename (build_dir :: fn_parts)
      in
        mkdir ~parent:true fn;
        fn
    in

      {
        build_dir     = build_dir;
        ocaml_lib_dir = mkdir_return ["lib"; "ocaml"];
        bin_dir       = mkdir_return ["bin"];
        lib_dir       = mkdir_return ["lib"];
        data_dir      = mkdir_return ["share"];
        doc_dir       = mkdir_return ["share"; "doc"];
        html_dir      = mkdir_return ["share"; "doc"; "html"];
      }
  in

  (* Run standard test *)
  let test_of_vector (srcdir, 
                      long,
                      oasis_extra_files,
                      installed_files,
                      post_install_runs) =
    srcdir >::
    bracket
      (* Setup test environment *)
      (fun () ->
         let cur_dir = 
           pwd ()
         in

         (* Create build dir *)
         let build_dir = 
           temp_dir "oasis-" ".dir"
         in

         let pristine = 
           (* Change to srcdir directory *)
           Sys.chdir srcdir;

           (* Ensure that we are in a clean environment *)
           rm oasis_extra_files;
           rm oasis_std_files;

           (* Memorize file listing/digest of the current srcdir *)
           all_file_digests ()
         in

           cur_dir, 
           (mkloc build_dir),
           pristine)

      (* Run test *)
      (fun (cur_dir, loc, pristine) ->
         let () = 
           skip_if 
             (long && not ctxt.long)
             "Long test"
         in

         let expected_post_oasis_files = 
           List.fold_left
             (fun st e ->
                SetFile.add e st)
             (set_file_of_file_digest pristine)
             (List.rev_map 
                (FilePath.make_absolute (pwd ()))
                (List.rev_append
                  oasis_std_files
                  oasis_extra_files))
         in

         let expected_installed_files loc = 
           (* Gather all file into a set *)
           List.fold_left
             (fun st e -> SetFile.add e st)
             SetFile.empty
             (* Compute all file that should have been installed *)
             (List.fold_left
                (fun acc f -> f loc acc)
                []
                installed_files)
         in

         (* Create build system using OASIS *)
         let () = 
           assert_command 
             ctxt
             ctxt.oasis 
             ctxt.oasis_args;

           (* Check generated files *)
           OUnitSetFile.assert_equal 
             ~msg:"Generated files"
             ~printer:fn_printer
             expected_post_oasis_files
             (all_files_cwd ())
         in

         (* Run configure target *)
         let () = 
           assert_run_setup 
             ["-configure"; 
              "--prefix";  loc.build_dir;
              "--docdir";  loc.doc_dir;
              "--htmldir"; loc.html_dir;
             ];

           assert_bool
             "File 'setup.data' has been created"
             (Sys.file_exists "setup.data")
         in

         (* Run build target *)
         let () = 
           assert_run_setup ["-build"]
         in

         (* Run test target *)
         let () = 
           assert_run_setup ["-test"]
         in

         (* Run documentation target *)
         let () = 
           assert_run_setup ["-doc"]
         in

         (* Generic function to run install/test/uninstall *)
         let install_test_uninstall ?(extra_env=[]) id loc test = 
           let extra_env =
             ("OCAMLFIND_DESTDIR", loc.ocaml_lib_dir)
             ::
             extra_env
           in
             (* Install *)
             assert_run_setup ~extra_env ["-install"];

             (* Check that we have installed everything as expected *)
             OUnitSetFile.assert_equal
               ~msg:(Printf.sprintf "Installed files (%s)" id)
               ~printer:(fn_printer ~root:loc.build_dir)
               (expected_installed_files loc)
               (all_files loc.build_dir);

             (* Test that installed files are working *)
             test ();

             (* Uninstall *)
             assert_run_setup ~extra_env ["-uninstall"];
             (* Check that no more files present in build_dir *)
             OUnitSetFile.assert_equal
               ~msg:(Printf.sprintf 
                       "Build directory is empty after uninstall (%s)" 
                       id)
               ~printer:(fn_printer ~root:loc.build_dir)
               SetFile.empty
               (all_files loc.build_dir)
         in

         (* Run install/uninstall target *)
         let () = 
           install_test_uninstall 
             "1st time"
             loc
             (fun () ->
                (* Test that installed files are working *)
                List.iter 
                  (fun f -> f ())
                  (List.fold_left
                     (fun acc f -> f loc acc)
                     []
                     post_install_runs))
         in

         (* Run install/uninstall target 2nd time *)
         let () = 
           install_test_uninstall 
             "2nd time"
             loc
             (fun () ->
                (* Test that installed files are working *)
                List.iter 
                  (fun f -> f ())
                  (List.fold_left
                     (fun acc f -> f loc acc)
                     []
                     post_install_runs))
         in

         (* Run install/uninstall target with destdir *)
         let () = 
           (* Prepending something at the beginning of a Win32 path
            * doesn't work because it will create a filename like:
            * c:\a\b\c:\c, which is illegal
            * TODO: find a solution for DESTDIR on Win32
            *)
           if Sys.os_type <> "Win32" then
             begin
               let destdir = 
                 loc.build_dir
               in
               let loc = 
                 mkloc 
                   (Filename.concat destdir 
                      (FilePath.make_relative 
                        (if Sys.os_type = "Win32" then
                          begin
                            if String.length loc.build_dir >= 2 then
                              String.sub loc.build_dir 0 2
                            else
                              failwith 
                                (Printf.sprintf
                                  "Cannot extract drive letter of filename '%s'" 
                                  loc.build_dir)
                          end
                        else
                          "/")
                        loc.build_dir))
               in
                 install_test_uninstall 
                   ~extra_env:["destdir", destdir]
                   "with destdir"
                   loc
                   ignore
             end
         in

         (* Run clean target *)
         let () = 
           assert_run_setup ["-clean"];
           assert_run_setup ["-distclean"];

           (* Check that only OASIS generated files remain *)
           OUnitSetFile.assert_equal
             ~msg:"Remaining files after distclean"
             ~printer:fn_printer
             expected_post_oasis_files
             (all_files_cwd ())
         in

         (* Clean test environment -- the standard way *)
         let () =
           rm oasis_std_files;
           rm oasis_extra_files;

           (* Check that we are back to pristine ls
            *)
           OUnitSetFileDigest.assert_equal
             ~msg:"Source directory back to pristine"
             ~printer:(fun (fn, dgst) ->
                         Printf.sprintf 
                           "'%s(%s)'" 
                           (FilePath.make_relative (pwd ()) fn)
                           (Digest.to_hex dgst))
             pristine
             (all_file_digests ())
         in

           ()
      )

      (* Clean test environment -- the backup way *)
      (fun (cur_dir, loc, pristine) ->

         let st_pristine = 
           set_file_of_file_digest pristine
         in
           (* Remove what was not here *)
           find
             Is_file
             (pwd ())
             (fun () fn ->
                if not (SetFile.mem fn st_pristine) then
                  rm [fn])
             ();

           rm ~recurse:true ["_build"];
           rm ["setup.data"; "setup.log"];

           (* Back into current dir *)
           Sys.chdir cur_dir;

           (* Destroy build directory *)
           rm ~recurse:true [loc.build_dir]
      )
  in

    "TestFull" >:::
    (List.map test_of_vector
       [
         (* Use flags *)
         "../examples/flags", 
         false,
         oasis_ocamlbuild_files
         @
         [
           "src/simplelib/simplelib.mllib";
           "src/simplelib/simplelib.odocl";
           "src/simplelibext/simplelibext.mllib";
           "src/simplelibext/simplelibext.odocl";
         ],
         [
           (in_ocaml_library "simplelib") 
             ["simplelib.cma"; 
              "Foo.cmi"; "Foo.ml"; 
              "Bar.cmi"; "Bar.ml"; 
              "META"];
           conditional 
             ctxt.has_ocamlopt
             (in_ocaml_library "simplelib" 
                ["simplelib.cmxa"; 
                 if Sys.os_type = "Win32" then
                   "simplelib.lib"
                 else
                   "simplelib.a"]);

           in_ocaml_library "simplelibext"
             ["simplelibext.cma"; 
              "FooExt.cmi"; "FooExt.ml"; 
              "BarExt.cmi"; "BarExt.ml"; 
              "META"];
           conditional
             ctxt.has_ocamlopt
             (in_ocaml_library "simplelibext"
                ["simplelibext.cmxa"; 
                 if Sys.os_type = "Win32" then 
                   "simplelibext.lib"
                 else
                   "simplelibext.a"]);

           api_ref_html "simplelib"
             ["Foo"; "Bar"];

           api_ref_html "simplelibext"
             ["FooExt"; "BarExt"];
         ],
         [
           (* TODO: test *)
         ];


         (* Complete library *)
         "../examples/simplelib", 
         true,
         oasis_ocamlbuild_files @ 
         [
           "src/simplelib.mllib";
           "src/simplelib.odocl";
         ],
         [
           in_ocaml_library "simplelib" 
             ["simplelib.cma"; 
              "foo.cmi"; "foo.mli"; 
              "bar.cmi"; "bar.mli"; 
              "META"];
           conditional 
             ctxt.has_ocamlopt
             (in_ocaml_library "simplelib"
                ["simplelib.cmxa"; 
                 if Sys.os_type = "Win32" then
                   "simplelib.lib"
                 else
                   "simplelib.a"]);

           api_ref_html "simplelib"
             ["Bar"; "Foo"];
         ],
         [
           (* TODO: test *)
         ];


         (* Complete library with findlib package to check *)
         "../examples/findlib",
         true,
         oasis_ocamlbuild_files,
         [],
         [
           (* TODO: test *)
         ];


         (* Complete library with custom build system *)
         "../examples/custom", 
         true,
         [],
         [
           in_ocaml_library "simplelib"
             ["simplelib.cma"; 
              "foo.cmi"; "foo.mli"; 
              "bar.cmi"; "bar.mli"; 
              "META"];
         ],
         [
           (* TODO: test *)
         ];


         (* Library/executable using C files *)
         "../examples/with-c",
         true,
         [
           "src/META"; 
           "src/libtest-with-c-custom.clib"; 
           "src/libtest-with-c-native.clib";
           "src/libtest-with-c.clib";
           "src/libwith-c.clib";
           "src/with-c.mllib";
           "src/with-c.odocl";
         ] @ oasis_ocamlbuild_files,
         [
           
           in_bin 
             (if Sys.os_type = "Win32" then
                ["test-with-c.exe"; "test-with-c-custom.exe"]
              else
                ["test-with-c"; "test-with-c-custom"]);
           conditional
             ctxt.has_ocamlopt
             (in_bin [if Sys.os_type = "Win32" then
                        "test-with-c-native.exe"
                      else
                        "test-with-c-native"]);
           in_library [if Sys.os_type = "Win32" then
                         "with-c/dlltest-with-c.dll"
                       else
                         "with-c/dlltest-with-c.so"];
           in_ocaml_library "with-c"
             ["A.cmi"; "A.ml"; "META"; "with-c.cma"]; 
           in_ocaml_library "with-c"
             (if Sys.os_type = "Win32" then
                ["libwith-c.lib"; "dllwith-c.dll"]
              else
                ["libwith-c.a"; "dllwith-c.so"]);
           conditional
             ctxt.has_ocamlopt
             (in_ocaml_library "with-c" 
                [if Sys.os_type = "Win32" then
                   "with-c.lib"
                 else
                   "with-c.a"; 
                 "with-c.cmxa"]);

           api_ref_html "with-c"
             ["A"];
           in_html "with-c"
             ["code_VALA.ident.html"];
         ],
         (if ctxt.has_ocamlopt then
            (fun lst ->  
               (try_installed_exec "test-with-c-native" [])
               ::
               lst)
          else
            (fun lst -> lst))
           [
             try_installed_exec "test-with-c-custom" [];
             try_installed_exec "test-with-c" [];
             try_installed_library "with-c" ["A"];
           ];

         (* Library/executable using data files *)
         "../examples/with-data",
         true,
         [
           "src/META";
           "src/test.mllib";
           "src/test.odocl";
         ] @ oasis_ocamlbuild_files,
         [
           in_bin [if Sys.os_type = "Win32" then
                     "test.exe"
                   else
                     "test"];
           in_ocaml_library "test"
             [
               "test.ml"; "test.cmi"; "META"; "test.cma";
             ];
           in_data_dir 
             ["with-data/test.txt"; 
              "doc/html/test.html";
              "with-data-0.1/test.txt"];

           api_ref_html "test"
             ["Test"];
         ],
         [
           try_installed_library "test" ["Test"];
         ];

         (* Test executable *)
         "../examples/with-test",
         true,
         oasis_ocamlbuild_files,
         [],
         [];

         (* Use sub-packages *)
         "../examples/with-subpackage",
         true,
         [
           "src/META";
           "src/test.mllib";
           "src/test.odocl";
           "src/syntax/pa_test.mllib";
         ] @ oasis_ocamlbuild_files,
         [
           in_ocaml_library "test" 
             ["META"; "test.cma"; "pa_test.cma";
              "A.ml"; "A.cmi"; "B.ml"; "B.cmi";
              "pa_test.ml"; "pa_test.cmi"];
           conditional 
             ctxt.has_ocamlopt
             (in_ocaml_library "test"
                ["test.cmxa"; 
                 if Sys.os_type = "Win32" then
                   "test.lib"
                 else
                   "test.a"]);

           api_ref_html "test"
             ["A"; "B"];
         ],
         [
           try_installed_library "test" ["A"; "B"];
         ];

         (* Interdependencies *)
         "../examples/interdepend-libraries",
         true,
         [
           "src/liba/liba.mllib";
           "src/liba/liba.odocl";
           "src/libb/libb.mllib";
           "src/libb/libb.odocl";
           "src/libc/libc.mllib";
           "src/libc/libc.odocl";
           "src/libd/libd.mllib";
           "src/libd/libd.odocl";
           "src/libe/libe.mllib";
           "src/libe/libe.odocl";
         ] @ oasis_ocamlbuild_files,
         [],
         [];

         (* Build order *)
         "../examples/order-matter",
         true,
         [
           "src/foo/foo.mllib";
           "src/foo/foo.odocl";
           "src/bar/bar.mllib";
           "src/bar/bar.odocl";
           "src/baz/baz.mllib";
           "src/baz/baz.odocl";
         ] @ oasis_ocamlbuild_files,
         [],
         [];

         (* Single level package *)
         "data/1level",
         true,
         [
           "META";
           "with-a.mllib";
           "with-a.odocl";
         ] @ oasis_ocamlbuild_files,
         [
           in_ocaml_library "with-a"
             ["META"; "A.ml"; "A.cmi"; "with-a.cma"];
           conditional
             ctxt.has_ocamlopt
             (in_ocaml_library "with-a"
                ["with-a.cmxa"; 
                 if Sys.os_type = "Win32" then
                   "with-a.lib"
                 else
                   "with-a.a"]);
           in_bin [if Sys.os_type = "Win32" then
                     "test-with-a.exe"
                   else
                     "test-with-a"];
           api_ref_html "with-a" ["A"];
         ],
         [
           try_installed_library "with-a" ["A"];
           try_installed_exec "test-with-a" [];
         ];

         (* Try custom document build *)
         "data/customdoc",
         true,
         ["META"; "with-a.mllib"] @ oasis_ocamlbuild_files,
         [
           in_ocaml_library "with-a"
             ["META"; "A.ml"; "A.cmi"; "with-a.cma"];
         ],
         [];
       ]
    )
;;
