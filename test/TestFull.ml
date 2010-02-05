
(** Run full OASIS use cases
    @author Sylvain Le Gall
  *)

open TestCommon;;
open FileUtil;;
open OUnit;;

type filename = FilePath.filename;;

module SetFileDigest = 
  Set.Make
    (struct
       type t = filename * Digest.t

       let compare a b =
         compare a b
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
module OUnitSetString = OUnitSet(SetString);;

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

  let set_string_of_file_digest st =
    SetFileDigest.fold
      (fun (fn, _) st ->
         SetString.add fn st)
      st
      SetString.empty
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
    let paths = 
      loc.bin_dir 
      ::
      (try
         FilePath.path_of_string (Unix.getenv "PATH")
       with Not_found ->
         [])
    in
    let ld_library_paths = 
      loc.lib_dir 
      ::
      (find 
         Is_dir
         loc.ocaml_lib_dir
         (fun acc fn -> fn :: acc)
         (try
            FilePath.path_of_string (Unix.getenv "LD_LIBRARY_PATH")
          with Not_found ->
            []))
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
          ~extra_env:["OCAMLPATH", 
                      loc.ocaml_lib_dir;
                      "LD_LIBRARY_PATH", 
                      FilePath.string_of_path ld_library_paths;
                      "PATH", 
                      FilePath.string_of_path paths]
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
      (fun st fn -> SetString.add fn st)
      SetString.empty
  in

  (* List all files in current working dir *)
  let all_files_cwd () = 
    all_files (pwd ())
  in

  (* Create a set of file/digest of the current directory *)
  let all_file_digests () = 
    SetString.fold
      (fun fn st ->
         SetFileDigest.add (fn, Digest.file fn) st)
      (all_files_cwd ())
      SetFileDigest.empty
  in

  (* Print a short version of the filename *)
  let fn_printer ?(root = pwd ()) fn =
    Printf.sprintf "'%s'" (FilePath.make_relative root fn)
  in


  (* Run standard test *)
  let test_of_vector (srcdir, 
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

         (* Create a directory in build_dir and return its name *)
         let mkdir_return ?(parent=true) fn_parts =
           let fn = 
             FilePath.make_filename (build_dir :: fn_parts)
           in
             mkdir ~parent fn;
             fn
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
           {
             build_dir     = build_dir;
             ocaml_lib_dir = mkdir_return ["lib"; "ocaml"];
             bin_dir       = mkdir_return ["bin"];
             lib_dir       = mkdir_return ["lib"];
             data_dir      = mkdir_return ["share"];
             doc_dir       = mkdir_return ["share"; "doc"];
           }, 
           pristine)

      (* Run test *)
      (fun (cur_dir, loc, pristine) ->

         let expected_post_oasis_files = 
           OASISUtils.set_string_add_list
             (set_string_of_file_digest pristine)
             (List.rev_map 
                (FilePath.make_absolute (pwd ()))
                (List.rev_append
                  oasis_std_files
                  oasis_extra_files))
         in

         (* Create build system using OASIS *)
         let () = 
           assert_command 
             ctxt
             ctxt.oasis 
             ctxt.oasis_args;

           (* Check generated files *)
           OUnitSetString.assert_equal 
             ~msg:"Generated files"
             ~printer:fn_printer
             expected_post_oasis_files
             (all_files_cwd ())
         in

         (* Run configure target *)
         let () = 
           assert_run_setup ["-configure"; "--prefix"; loc.build_dir];

           assert_bool
             "File 'setup.data' has been created"
             (Sys.file_exists "setup.data");
         in

         (* Run build target *)
         let () = 
           assert_run_setup ["-build"];
         in

         (* Run test target *)
         let () = 
           assert_run_setup ["-test"];
         in

         (* Run documentation target *)
         let () = 
           (* TODO: activate *)
           (*assert_run_setup ["-documentation"];*)
           ()
         in

         (* Run install target *)
         let () = 
           let expected_installed_files = 
             (* Gather all file into a set *)
             OASISUtils.set_string_of_list
               (* Compute all file that should have been installed *)
               (List.fold_left
                  (fun acc f -> f loc acc)
                  []
                  installed_files)
           in
             assert_run_setup
               ~extra_env:["OCAMLFIND_DESTDIR", loc.ocaml_lib_dir]
               ["-install"];

             (* Check that we have installed everything as expected *)
             OUnitSetString.assert_equal
               ~msg:"Installed files"
               ~printer:(fn_printer ~root:loc.build_dir)
               expected_installed_files
               (all_files loc.build_dir);

             (* Test that installed files are working *)
             List.iter 
               (fun f -> f ())
               (List.fold_left
                  (fun acc f -> f loc acc)
                  []
                  post_install_runs)
         in

         (* Run uninstall target *)
         let () = 
           assert_run_setup 
               ~extra_env:["OCAMLFIND_DESTDIR", loc.ocaml_lib_dir]
               ["-uninstall"];
           (* Check that no more files present in build_dir *)
           OUnitSetString.assert_equal
             ~msg:"Build directory is empty after uninstall"
             ~printer:(fn_printer ~root:loc.build_dir)
             SetString.empty
             (all_files loc.build_dir)
         in

         (* Run clean target *)
         let () = 
           assert_run_setup ["-clean"];
           assert_run_setup ["-distclean"];

           (* Check that only OASIS generated files remain *)
           OUnitSetString.assert_equal
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
           set_string_of_file_digest pristine
         in
           (* Remove what was not here *)
           find
             Is_file
             (pwd ())
             (fun () fn ->
                if not (SetString.mem fn st_pristine) then
                  rm [fn])
             ();

           rm ~recurse:true ["_build"];
           rm ["setup.data"];

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
         oasis_ocamlbuild_files
         @
         [
           "src/simplelib/simplelib.mllib";
           "src/simplelibext/simplelibext.mllib";
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
                ["simplelib.cmxa"; "simplelib.a"]);

           in_ocaml_library "simplelibext"
             ["simplelibext.cma"; 
              "FooExt.cmi"; "FooExt.ml"; 
              "BarExt.cmi"; "BarExt.ml"; 
              "META"];
           conditional
             ctxt.has_ocamlopt
             (in_ocaml_library "simplelibext"
                ["simplelibext.cmxa"; "simplelibext.a"]);
         ],
         [
           (* TODO: test *)
         ];


         (* Complete library *)
         "../examples/simplelib", 
         oasis_ocamlbuild_files @ ["src/simplelib.mllib"],
         [
           in_ocaml_library "simplelib" 
             ["simplelib.cma"; 
              "foo.cmi"; "foo.mli"; 
              "bar.cmi"; "bar.mli"; 
              "META"];
           conditional 
             ctxt.has_ocamlopt
             (in_ocaml_library "simplelib"
                ["simplelib.cmxa"; "simplelib.a"]);
         ],
         [
           (* TODO: test *)
         ];


         (* Complete library with findlib package to check *)
         "../examples/findlib",
         oasis_ocamlbuild_files,
         [],
         [
           (* TODO: test *)
         ];


         (* Complete library with custom build system *)
         "../examples/custom", 
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
         [
           "src/META"; 
           "src/libtest-with-c-custom.clib"; 
           "src/libtest-with-c-native.clib";
           "src/libtest-with-c.clib";
           "src/libwith-c.clib";
           "src/with-c.mllib";
         ] @ oasis_ocamlbuild_files,
         [
           in_bin ["test-with-c"; 
                   "test-with-c-custom"; 
                   "test-with-c-native"];
           in_library ["dlltest-with-c.so"];
           in_ocaml_library "with-c"
             [
               "A.cmi"; "A.ml"; "META"; "with-c.cma";
               "libwith-c.a"; "dllwith-c.so"
             ]; 
           conditional
             ctxt.has_ocamlopt
             (in_ocaml_library "with-c" 
                ["with-c.a"; "with-c.cmxa"]);
         ],
         [
           try_installed_exec "test-with-c-native" [];
           try_installed_exec "test-with-c-custom" [];
           try_installed_exec "test-with-c" [];
           try_installed_library "with-c" ["A"];
         ];

         (* Library/executable using data files *)
         "../examples/with-data",
         [
           "src/META";
           "src/test.mllib";
         ] @ oasis_ocamlbuild_files,
         [
           in_bin ["test"];
           in_ocaml_library "test"
             [
               "test.ml"; "test.cmi"; "META"; "test.cma";
             ];
           in_data_dir 
             ["with-data/test.txt"; 
              "doc/with-data/test.html";
              "with-data-0.1/test.txt"];
         ],
         [
           try_installed_library "test" ["Test"];
         ];

         (* Test executable *)
         "../examples/with-test",
         oasis_ocamlbuild_files,
         [],
         [];

         (* Use sub-packages *)
         "../examples/with-subpackage",
         [
           "src/META";
           "src/test.mllib";
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
                ["test.cmxa"; "test.a"]);
         ],
         [
           try_installed_library "test" ["A"; "B"];
         ];
       ]
    )
;;
