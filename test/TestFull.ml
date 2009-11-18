
(** Run full OCamlAutobuild use case
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

  (* Assert checking that command run well *)
  let assert_command ?(exit_code=0) ?(extra_env=[]) cmd args  =
    let cmdline =
      String.concat " " (cmd :: args)
    in
    let fn, chn_out =
      Filename.open_temp_file "ocaml-autobuild-" ".log"
    in
    let chn_in =
      open_in fn
    in
    let () = 
      Sys.remove fn
    in
    let fd =
      Unix.descr_of_out_channel chn_out
    in
    let pid =
      Unix.create_process_env 
        cmd 
        (Array.of_list (cmd :: args))
        (Array.concat 
           [
             Unix.environment ();
             Array.of_list (List.map (fun (nm, vl) -> nm^"="^vl) extra_env);
           ])
        Unix.stdin
        fd
        fd;
    in
    let dump_stdout_stderr () = 
      let buff =
        Buffer.create 13
      in
        close_out chn_out;
        Buffer.add_channel buff chn_in (in_channel_length chn_in);
        close_in chn_in;
        Printf.eprintf "Error running command '%s'\n%!" cmdline;
        Buffer.output_buffer stderr buff;
        flush stderr
    in
      match Unix.waitpid [] pid with
        | _, Unix.WEXITED i ->
            if i <> exit_code then
              dump_stdout_stderr ();
            assert_equal
              ~msg:"exit code"
              ~printer:string_of_int
              exit_code
              i;
            close_in chn_in;
            close_out chn_out
        | _, Unix.WSIGNALED i ->
            dump_stdout_stderr ();
            failwith 
              (Printf.sprintf
                 "Process '%s' has been killed by signal %d"
                 cmdline
                 i)
        | _, Unix.WSTOPPED i ->
            dump_stdout_stderr ();
            failwith
              (Printf.sprintf
                 "Process '%s' has been stopped by signal %d"
                 cmdline
                 i)
  in

  (* Assert with setup.ml *)
  let assert_run_setup ?exit_code ?extra_env args =
    assert_command ?exit_code ?extra_env "ocaml" ("setup.ml" :: args)
  in

  (* Files always generated *)
  let autobuild_std_files = 
    [
      "setup.ml"; 
    ]
  in

  (* Files generated when ocamlbuild buildsys is used *)
  let autobuild_ocamlbuild_files =
    [
      "myocamlbuild.ml"; 
      "_tags";
    ]
  in

  (* Set all file location into build_dir + ocaml_lib_dir + library *) 
  let in_ocaml_library lib files loc acc = 
    List.fold_left
      (fun acc fn ->
         (FilePath.make_filename [loc.ocaml_lib_dir; lib; fn])
         ::
         acc)
      acc
      files
  in

    (*
  (* Set all file location into bin_dir *)
  let in_bin files loc acc =
    List.fold_left
      (fun fn acc ->
         (FilePath.concat loc.bin_dir fn) :: acc)
      files
      acc
  in
     *)

  (* Run target conditionally *)
  let conditional cond f loc acc =
    if cond then
      f loc acc
    else
      acc
  in

  (* Create a set of file/digest of the current directory *)
  let build_pristine_ls () = 
    find 
      Is_file
      (pwd ())
      (fun st fn ->
         SetFileDigest.add (fn, Digest.file fn) st)
      SetFileDigest.empty
  in

  (* Run standard test *)
  let test_of_vector (srcdir, 
                      autobuild_extra_files,
                      installed_files)  =
    srcdir >::
    bracket
      (* Setup test environment *)
      (fun () ->
         let cur_dir = 
           pwd ()
         in

         let build_dir = 
           (* Create build dir *)
           let res = 
             Filename.temp_file "ocaml-autobuild-" ".dir"
           in
             rm [res];
             mkdir res;
             res
         in

         (* Create a directory in build_dir and return its name *)
         let mkdir_return ?(parent=true) fn_parts =
           let fn = 
             FilePath.make_filename (build_dir :: fn_parts)
           in
             mkdir ~parent fn;
             fn
         in

         let pristine_ls = 
           (* Change to srcdir directory *)
           Sys.chdir srcdir;

           (* Ensure that we are in a clean environment *)
           rm autobuild_extra_files;
           rm autobuild_std_files;

           (* Memorize file listing/digest of the current srcdir *)
           build_pristine_ls ()
         in

           cur_dir, 
           {
             build_dir     = build_dir;
             ocaml_lib_dir = mkdir_return ["lib"; "ocaml"];
             bin_dir       = mkdir_return ["bin"];
           }, 
           pristine_ls)

      (* Run test *)
      (fun (cur_dir, loc, pristine_ls) ->
         let ocaml_autobuild = 
           FilePath.concat cur_dir "../_build/src/OCamlAutobuild.byte"
         in

         (* Create build system using OCamlAutobuild *)
         let () = 
           assert_command ocaml_autobuild [];

           (* Check generated files *)
           List.iter
             (fun fn -> 
                assert_bool
                  (Printf.sprintf "File '%s' is not generated" fn)
                  (Sys.file_exists fn))
             (autobuild_std_files @ autobuild_extra_files);
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
           (* TODO: activate *)
           (*assert_run_setup ["-test"];*)
           ()
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
             List.fold_left
               (fun st fn -> SetString.add fn st)
               SetString.empty

               (* Compute all file that should have been installed *)
               (List.fold_left
                  (fun acc f -> f loc acc)
                  []
                  installed_files)
           in

           let () =
             assert_run_setup
               ~extra_env:["OCAMLFIND_DESTDIR", loc.ocaml_lib_dir]
               ["-install"];
           in

           let really_installed_files =
             (* Find every files installed *)
             find 
               Is_file
               loc.build_dir
               (fun st fn ->
                  SetString.add fn st)
               SetString.empty
           in

             (* Check that we have installed everything as expected *)
             OUnitSetString.assert_equal
               ~msg:"Installed files"
               ~printer:(fun fn -> 
                           Printf.sprintf "'%s'" 
                             (FilePath.make_relative loc.build_dir fn))
               expected_installed_files
               really_installed_files
         in

         (* Run clean target *)
         let () = 
           assert_run_setup ["-clean"];
           assert_run_setup ["-distclean"]
         in

         (* Clean test environment -- the standard way *)
         let () =
           rm autobuild_std_files;
           rm autobuild_extra_files;

           (* Check that we are back to pristine ls
            *)
           OUnitSetFileDigest.assert_equal
             ~msg:"Source directory back to pristine"
             ~printer:(fun (fn, dgst) ->
                         Printf.sprintf "'%s(%s)'" fn (Digest.to_hex dgst))
             pristine_ls
             (build_pristine_ls ())
         in

           ()
      )

      (* Clean test environment -- the backup way *)
      (fun (cur_dir, loc, pristine_ls) ->

         let st_pristine = 
           SetFileDigest.fold
             (fun (fn, _) st ->
                SetString.add fn st)
             pristine_ls
             SetString.empty
         in
           (* Remove what was not here *)
           find
             Is_file
             (pwd ())
             (fun () fn ->
                if not (SetString.mem fn st_pristine) then
                  rm [fn])
             ();

           (* Back into current dir *)
           Sys.chdir cur_dir;

           (* Destroy build directory *)
           rm ~recurse:true [loc.build_dir])
  in

    "TestFull" >:::
    (List.map test_of_vector
       [
         "../examples/flags", 
         autobuild_ocamlbuild_files
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
             ((in_ocaml_library "simplelib") 
                ["simplelib.cmxa"; "simplelib.a"]);

           (in_ocaml_library "simplelibext") 
             ["simplelibext.cma"; 
              "FooExt.cmi"; "FooExt.ml"; 
              "BarExt.cmi"; "BarExt.ml"; 
              "META"];
           conditional
             ctxt.has_ocamlopt
             ((in_ocaml_library "simplelibext") 
                ["simplelibext.cmxa"; "simplelibext.a"]);
         ];

         "../examples/simplelib", 
         autobuild_ocamlbuild_files @ ["src/simplelib.mllib"],
         [
           in_ocaml_library "simplelib" 
             ["simplelib.cma"; 
              "foo.cmi"; "foo.mli"; 
              "bar.cmi"; "bar.mli"; 
              "META"];
           conditional 
             ctxt.has_ocamlopt
             ((in_ocaml_library "simplelib") 
                ["simplelib.cmxa"; "simplelib.a"]);
         ];

         "../examples/findlib",
         autobuild_ocamlbuild_files,
         [];

         "../examples/custom", 
         [],
         [
           in_ocaml_library "simplelib"
             ["simplelib.cma"; 
              "foo.cmi"; "foo.mli"; 
              "bar.cmi"; "bar.mli"; 
              "META"];
         ];
       ])
;;
