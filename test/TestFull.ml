(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

(** Run full OASIS use cases
    @author Sylvain Le Gall
  *)

open FileUtil;;
open OUnit;;
open TestCommon;;

type filename = FilePath.filename;;

let exec fn = 
  if Sys.os_type = "Win32" then
    fn^".exe"
  else
    fn

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

let tests =

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

  let setup_ml = "setup.ml" in

  (* Assert with setup.ml *)
  let assert_run_setup ?exit_code ?extra_env args =
    (* Speed up for testing, compile setup.ml *)
    let can_compile =
      let chn = open_in setup_ml in
      let hash_load = ref false in
      let () =
        try
          while not !hash_load do
            if OASISString.starts_with ~what:"#load" (input_line chn) then
              hash_load := true
          done
        with End_of_file ->
          ()
      in
        close_in chn;
        not !hash_load
    in
    let setup_base = Filename.chop_extension setup_ml in
    let setup_digest = setup_base ^ ".digest" in
    let setup_exe = 
      Filename.concat Filename.current_dir_name (exec setup_base)
    in
      if can_compile then
        begin
          let self_digest =
            let chn = open_in setup_ml in
            let digest = Digest.channel chn (in_channel_length chn) in
              close_in chn;
              digest
          in
          let pre_digest =
            try
              let chn = open_in setup_digest in
              let digest =
                try
                  Digest.input chn 
                with _ ->
                  Digest.string ""
              in
                close_in chn;
                digest
            with _ ->
              Digest.string ""
          in
          let clean ?(all=false) () =
            List.iter
              (fun fn -> try Sys.remove fn with _ -> ())
              (
                (if all then
                   [setup_exe; setup_digest]
                 else
                   [])
                @
                [setup_base ^ ".cmi"; setup_base ^ ".cmo"]
              )
          in
          if not (Sys.file_exists setup_exe) || self_digest <> pre_digest then
            begin
              match Sys.command ("ocamlfind ocamlc -o "^(exec "setup")^" setup.ml") with
                | 0 ->
                    (* Compilation succeed, update the digest *)
                    let chn = open_out setup_digest in
                      Digest.output chn self_digest;
                      close_out chn;
                      clean ()
                | _ ->
                    prerr_endline "E: Compilation of setup.ml doesn't succeed.";
                    clean ~all:true ()
            end
        end;
      if Sys.file_exists setup_exe then
        assert_command ?exit_code ?extra_env setup_exe ("-info" :: "-debug" :: args)
      else
        assert_command ?exit_code ?extra_env "ocaml" (setup_ml :: "-info" :: "-debug" :: args)
  in

  (* Files always generated *)
  let oasis_std_files = 
    [
      setup_ml; 
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

  let add_path nm dir = 
    nm,
    try 
      FilePath.string_of_path
        ((FilePath.path_of_string (Sys.getenv nm)) @ [dir])
    with Not_found ->
      dir
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
          ~extra_env:((add_path "OCAMLPATH" loc.ocaml_lib_dir)
                      ::
                      (List.map
                         (fun (v, lst) ->
                            v, FilePath.string_of_path lst)
                         (("PATH", paths) :: extra_env)))
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
            OASISString.replace_chars
              (function
                 | '-' -> '_'
                 | c -> c)
              pkg
          in
          let fn = 
            FilePath.concat srcdir ("test_"^pkg_as_module^".ml")
          in
          let extra_env = 
            [add_path "OCAMLPATH" loc.ocaml_lib_dir]
          in
          let assert_compile cmd args =
            assert_command 
              ~extra_env 
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

            if !has_ocamlopt then
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

  let long_test () =
    skip_if (not !long) "Long test"
  in

  let filter_platform lst = 
    let check_suff fn suff = Filename.check_suffix fn ("."^suff) in
    List.fold_left 
      (fun acc fn -> 
         if (check_suff fn "cmx" || check_suff fn "cmxa") then
           if !has_ocamlopt then
             fn :: acc
           else
             acc
         else if check_suff fn "cmxs" then
           if !has_native_dynlink then
             fn :: acc
           else
             acc
         else if check_suff fn "a" then 
           let fn =
             if Sys.os_type = "Win32" then
               (Filename.chop_extension fn) ^ ".lib"
             else
               fn
           in
             if OASISString.starts_with (Filename.basename fn) "lib" then
               (* stubs library *)
               fn :: acc
             else if !has_ocamlopt then
               (* library matching the .cmxa *)
               fn :: acc
             else
               acc
         else if check_suff fn "so" then
           let fn = 
             if Sys.os_type = "Win32" then
               (Filename.chop_extension fn) ^ ".dll"
             else
               fn
           in
             fn :: acc
         else 
           fn :: acc)
      [] lst
  in
    
  let bracket_setup 
        ?(dev=false)
        ?(dynamic=false)
        (srcdir, vars)
        f = 
    bracket
      (fun () ->
         let (skip_cond, oasis_extra_files, _, post_install_runs) = 
           vars () 
         in

         let cur_dir = 
           pwd ()
         in

         (* Create build dir *)
         let build_dir = 
           temp_dir "oasis-" ".dir"
         in

         let () = 
           (* Change to srcdir directory *)
           Sys.chdir srcdir
         in

         (* Make a backup of already existing OASIS files *)
         let bak_lst = 
           List.fold_left
             (fun acc fn ->
                if Sys.file_exists fn then 
                  begin
                    let bak_fn = 
                      Filename.temp_file "oasis-" ".bak"
                    in
                      FileUtil.cp [fn] bak_fn;
                      (fn, bak_fn) :: acc
                  end
                else
                  begin
                    acc
                  end)
             []
             oasis_extra_files
         in

         let pristine = 
           (* Memorize file listing/digest of the current srcdir *)
           all_file_digests ()
         in

           cur_dir, 
           (mkloc build_dir),
           pristine, 
           bak_lst)

      (fun (cur_dir, loc, pristine, bak_lst) ->
         let (skip_cond, oasis_extra_files, _, post_install_runs) = 
           vars () 
         in

         let () = 
           skip_cond ()
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

         (* Create build system using OASIS *)
         let () = 
           assert_oasis_cli
             ("setup" ::
              (if dev then
                 ["-real-oasis"; "-setup-update";
                  if dynamic then "dynamic" else "weak"]
               else
                 []));

           if Sys.file_exists "_tags" && !dbug then
             begin
               let chn = open_in "_tags" in
                 print_endline "file _tags";
                 try 
                   while true do 
                     print_endline (input_line chn)
                   done
                 with End_of_file ->
                   close_in chn
             end;

           (* Fix #require in dynamic *)
           if dynamic then
             begin
               let chn = open_in "setup.ml" in
               let lst = ref [] in
               let mkload lst =
                 let cma =
                   Filename.concat
                     cur_dir
                     (FilePath.make_filename (".." :: "_build" :: "src" :: lst))
                 in
                 Printf.sprintf
                   "#load %S;;\n#directory %S;;" cma (Filename.dirname cma)
               in
               let () =
                 try
                   while true do
                     let line = input_line chn in
                       if OASISString.starts_with
                            ~what:"#require \"oasis.dynrun\";;" line then
                         begin
                           lst :=
                           List.rev_append
                             ("#require \"unix\";;" ::
                              "#require \"odn\";;" ::
                              (List.map mkload
                                 [["oasis"; "oasis.cma"];
                                  ["base"; "base.cma"];
                                  ["builtin-plugins.cma"];
                                  ["dynrun"; "dynrun.cma"]]))
                             !lst
                         end
                       else
                         lst := line :: !lst
                   done
                 with End_of_file ->
                   close_in chn
               in
               let chn = open_out "setup.ml" in
               let () = 
                 if !dbug then
                   print_endline "file setup.ml:";
                 List.iter
                   (fun line ->
                      if !dbug then
                        print_endline line;
                      output_string chn (line^"\n"))
                   (List.rev !lst);
                 close_out chn
               in
                 ()
             end;

           (* Check generated files *)
           OUnitSetFile.assert_equal 
             ~msg:"Generated files"
             ~printer:fn_printer
             expected_post_oasis_files
             (all_files_cwd ())
         in

        (* If a _tags file exists, try to test its content. *)
        let () =
          if Sys.file_exists "_tags" then
            let chn = open_in "_tags" in
            let lineno = ref 0 in
              try
                while true do
                  let line = input_line chn in
                  let () = incr lineno in
                    try
                      let _ = "(*" in
                      let substr = Pcre.exec ~pat:"^\\s*\"(.*)\"\\s*:" line in
                      let fn = Pcre.get_substring substr 1 in
                        if List.mem fn
                             [".git"; ".bzr"; ".hg"; "_darcs"] ||
                           List.mem (FilePath.get_extension fn)
                             ["byte"; "native"; "lib"; "dll"; "a"; "so"; "cmxs"] then
                          ()
                        else if FilePath.get_extension fn = "cmx" then
                          begin
                            let fn_ml = FilePath.replace_extension fn "ml" in
                              if not  (Sys.file_exists fn_ml) then
                                assert_failure
                                  (Printf.sprintf
                                     "source file '%s' doesn't exist" fn_ml)
                          end
                        else if not (Sys.file_exists fn) then
                          assert_failure
                            (Printf.sprintf "file '%s' doesn't exist in _tags file line %d (%S)" 
                               fn !lineno line)
                    with Not_found ->
                      (* TODO: handle ocamlbuild wildcard *)
                      ()
                done
              with End_of_file ->
                close_in chn
        in

           (* Run the main function *)
           f (cur_dir, loc, pristine, expected_post_oasis_files);

           (* Clean test environment -- the standard way *)
           rm oasis_std_files;
           rm oasis_extra_files;

           (* Restore backup file *)
           List.iter 
             (fun (fn, bak_fn) ->
                FileUtil.cp [bak_fn] fn;
                FileUtil.rm [bak_fn])
             bak_lst;

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
      )

      (* Clean test environment -- the backup way *)
      (fun (cur_dir, loc, pristine, bak_lst) ->

         (* Restore backup file *)
         let () = 
           List.iter 
             (fun (fn, bak_fn) ->
                if Sys.file_exists bak_fn then
                  begin
                    FileUtil.cp [bak_fn] fn;
                    FileUtil.rm [bak_fn]
                  end)
             bak_lst
         in

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

  (* Run short test *)
  let test_of_vector_short ?extra_env e =
    "ocaml setup.ml -all" >::
    bracket_setup e
      (fun _ -> 
         assert_run_setup ?extra_env ["-all"];
         assert_run_setup ?extra_env ["-distclean"];
         rm ~force:Force [exec "setup"; "setup.digest"])
  in

  (* Run standard test *)
  let test_of_vector_std ((_, f) as e) =
    "standard" >::
    bracket_setup e
      (* Run test *)
      (fun (cur_dir, loc, pristine, expected_post_oasis_files) ->
         let (_, _, installed_files, post_install_runs) = 
           f ()
         in

         let expected_installed_files loc = 
           (* Gather all file into a set *)
           List.fold_left
             (fun st e -> SetFile.add e st)
             SetFile.empty
             (* Compute all file that should have been installed *)
             (filter_platform
               (List.fold_left
                  (fun acc f -> f loc acc)
                  []
                  installed_files))
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

         (* Extract ocamlbuild flags and check that they are correct. *)
         let () = 
           if Sys.file_exists "myocamlbuild.ml" then
             let documentation_output = 
               let buf = Buffer.create 16000 in
               OUnit.assert_command 
                 ~foutput:(Stream.iter (Buffer.add_char buf))
                 "ocamlbuild" ["-documentation"];
               Buffer.contents buf
             in
             let lst = OASISUtils.split_newline documentation_output in
             let rst = ref SetString.empty in
             let () =
               List.iter 
                 (fun line ->
                    try
                      let _ = "(*" in
                      let substr = Pcre.exec ~pat:"flag {\\. (.*) \\.}" line in
                      let lst = 
                        Pcre.split ~pat:"\\s*,\\s*" 
                          (Pcre.get_substring substr 1)
                      in
                        rst :=
                        List.fold_left
                          (fun st e -> SetString.add e st)
                          !rst
                          lst
                    with Not_found ->
                      ())
                 lst
             in
               if !rst = SetString.empty then
                 assert_failure ("Set of flags should not be empty.");
               SetString.iter 
                 (fun flag ->
                    String.iter 
                      (function
                         | 'A'..'Z' | 'a'..'z' | '0'..'9' 
                         | '_' | '.' | ':' | '-' | '(' | ')' -> 
                             ()
                         | c ->
                             assert_failure 
                               (Printf.sprintf 
                                  "flag %S contains %C which is illegal."
                                  flag c))
                      flag)
                 !rst
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
             ("OCAMLFIND_LDCONF", "ignore")
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
           assert_run_setup ["-clean"];
           assert_run_setup ["-distclean"];
           rm ~force:Force [exec "setup"; "setup.digest"];

           (* Check that only OASIS generated files remain *)
           OUnitSetFile.assert_equal
             ~msg:"Remaining files after distclean"
             ~printer:fn_printer
             expected_post_oasis_files
             (all_files_cwd ())
      )

  in

  let test_of_vector ((srcdir, _) as e) =
    srcdir >:::
    [
      test_of_vector_std e;
      test_of_vector_short e;
    ]
  in

    "TestFull" >:::
    (List.map test_of_vector
       [
         (* Use flags *)
         "../examples/flags", 
         (fun () ->
            ignore,
            oasis_ocamlbuild_files
            @
            [
              "src/simplelib/simplelib.mllib";
              "src/simplelib/simplelib.odocl";
              "src/simplelibext/simplelibext.mllib";
              "src/simplelibext/simplelibext.odocl";
            ],
            [
              in_ocaml_library "simplelib"
                ["simplelib.cma"; 
                 "Foo.cmi"; "Foo.ml"; 
                 "Bar.cmi"; "Bar.ml"; 
                 "META";
                 "simplelib.cmxa"; 
                 "simplelib.cmxs";
                 "Foo.cmx"; "Bar.cmx";
                 "simplelib.a"];

              in_ocaml_library "simplelibext"
                ["simplelibext.cma"; 
                 "FooExt.cmi"; "FooExt.ml"; 
                 "BarExt.cmi"; "BarExt.ml"; 
                 "META";
                 "simplelibext.cmxa"; 
                 "simplelibext.cmxs";
                 "FooExt.cmx"; "BarExt.cmx";
                 "simplelibext.a"];

              api_ref_html "simplelib"
                ["Foo"; "Bar"];

              api_ref_html "simplelibext"
                ["FooExt"; "BarExt"];
            ],
            [
            ]);


         (* Complete library *)
         "../examples/simplelib", 
         (fun () -> 
            long_test,
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
                 "META";
                 "simplelib.cmxa"; 
                 "simplelib.cmxs";
                 "foo.cmx"; "bar.cmx";
                 "simplelib.a"];

              api_ref_html "simplelib"
                ["Bar"; "Foo"];
            ],
            []);

         (* Packed library *)
         "../examples/packedlib", 
         (fun () -> 
            long_test,
            oasis_ocamlbuild_files @ 
              [ "src/packedlib.mlpack"; "src/META" ],
            [
              in_ocaml_library "packedlib" 
                ["packedlib.cma"; "packedlib.cmi";
                 "foo.mli"; "bar.mli"; "Baz.ml"; "META";
                 "packedlib.cmxa"; "packedlib.cmxs";
                 "packedlib.a"];
            ],
            [
              try_installed_library "packedlib" ["Packedlib.Foo"]
            ]);

         (* Complete library with findlib package to check *)
         "../examples/findlib",
         (fun () -> 
            long_test,
            oasis_ocamlbuild_files,
            [],
            []);

         (* Complete library with custom build system *)
         "../examples/custom", 
         (fun () -> 
            long_test,
            [],
            [
              in_ocaml_library "simplelib"
                ["simplelib.cma"; 
                 "foo.cmi"; "foo.mli"; 
                 "bar.cmi"; "bar.mli"; 
                 "META"];
            ],
            []);

         (* Library/executable using C files *)
         "../examples/with-c",
         (fun () -> 
            long_test,
            [
              "src/META"; 
              "src/libtest-with-c-custom_stubs.clib";
              "src/libtest-with-c-native_stubs.clib";
              "src/libtest-with-c_stubs.clib";
              "src/libwith-c_stubs.clib";
              "src/with-c.mllib";
              "src/with-c.odocl";
            ] @ oasis_ocamlbuild_files,
            [
              in_bin [exec "test-with-c"; exec "test-with-c-custom"];
              conditional
                !has_ocamlopt
                (in_bin [exec "test-with-c-native"]);
              in_library ["with-c/dlltest-with-c_stubs.so"];
              in_ocaml_library "with-c"
                ["A.cmi"; "A.ml"; "META"; "with-c.cma"; 
                 "libwith-c_stubs.a"; "dllwith-c_stubs.so";
                 "with-c.a"; "A.cmx"; "with-c.cmxa"; "with-c.cmxs"];

              api_ref_html "with-c"
                ["A"];
              in_html "with-c"
                ["code_VALA.ident.html"];
            ],
         (if !has_ocamlopt then
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
           ]);

         (* Library/executable using data files *)
         "../examples/with-data",
         (fun () -> 
            long_test,
            [
              "src/META";
              "src/test.mllib";
              "src/test.odocl";
            ] @ oasis_ocamlbuild_files,
            [
              in_bin [exec "test"];
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
            ]);

         (* Library with a pure interface module in subdirectory. *)
         "../examples/with-interface-module",
         (fun () ->
            long_test,
            [
              "src/META";
              "src/pimlib.mllib";
            ] @ oasis_ocamlbuild_files,
            [
              in_ocaml_library "pimlib"
                ["META";
                 "pimlib.cma"; "pimlib.cmxa"; "pimlib.a"; "pimlib.cmxs";
                 "pim_impl.mli"; "pim_impl.cmi"; "pim_impl.cmx";
                 "pim_intf.mli"; "pim_intf.cmi";
                 "pim_types.mli"; "pim_types.cmi"];
            ],
            [
              try_installed_library "pimlib" ["Pim_intf"; "Pim_impl"];
            ]);

         (* Library with a pure interface module in top source directory. *)
         "../examples/with-interface-module/src",
         (fun () ->
            long_test,
            [
              "META";
              "pimlib.mllib";
            ] @ oasis_ocamlbuild_files,
            [
              in_ocaml_library "pimlib"
                ["META";
                 "pimlib.cma"; "pimlib.cmxa"; "pimlib.a"; "pimlib.cmxs";
                 "pim_impl.mli"; "pim_impl.cmi"; "pim_impl.cmx";
                 "pim_intf.mli"; "pim_intf.cmi";
                 "pim_types.mli"; "pim_types.cmi"];
            ],
            [
              try_installed_library "pimlib" ["Pim_intf"; "Pim_impl"];
            ]);

         (* Test executable *)
         "../examples/with-test",
         (fun () ->
            long_test,
            oasis_ocamlbuild_files,
            [],
            []);

         (* Use sub-packages *)
         "../examples/with-subpackage",
         (fun () -> 
           long_test,
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
                "pa_test.ml"; "pa_test.cmi";
                "test.cmxa"; "test.cmxs"; "A.cmx"; "B.cmx";
                "test.a"];

             api_ref_html "test"
               ["A"; "B"];
           ],
           [
             try_installed_library "test" ["A"; "B"];
           ]);

         (* Interdependencies *)
         "../examples/interdepend-libraries",
         (fun () ->
            long_test,
            [
              "src/interdepend.odocl";
              "src/liba/liba.mllib";
              "src/libb/libb.mllib";
              "src/libc/libc.mllib";
              "src/libd/libd.mllib";
              "src/libe/libe.mllib";
            ] @ oasis_ocamlbuild_files,
            [],
            []);

         (* Build order *)
         "../examples/order-matter",
         (fun () -> 
            long_test,
            [
              "src/order-matter.odocl";
              "src/foo/foo.mllib";
              "src/bar/bar.mllib";
              "src/baz/baz.mllib";
            ] @ oasis_ocamlbuild_files,
            [],
            []);

         (* Single level package *)
         "data/1level",
         (fun () ->
            long_test,
            [
              "META";
              "with-a.mllib";
              "with-a.odocl";
            ] @ oasis_ocamlbuild_files,
            [
              in_ocaml_library "with-a"
                ["META"; "A.ml"; "A.cmi"; "with-a.cma";
                 "A.cmx"; "with-a.cmxa"; "with-a.cmxs"; 
                 "with-a.a"];
              in_bin [exec "test-with-a"];
              api_ref_html "with-a" ["A"];
            ],
            [
              try_installed_library "with-a" ["A"];
              try_installed_exec "test-with-a" [];
            ]);

         (* Try custom document build *)
         "data/customdoc",
         (fun () -> 
            long_test,
            ["META"; "with-a.mllib"] @ oasis_ocamlbuild_files,
            [
              in_ocaml_library "with-a"
                ["META"; "A.ml"; "A.cmi"; "with-a.cma"];
            ],
            []);

         (* Use cclib option *)
         "data/with-cclib",
         (fun () ->
            (fun () ->
               long_test ();
               skip_if 
                 (not (Sys.file_exists "/usr/include/stringprep.h"))
                 "Cannot find 'stringprep.h'"),
            ["src/META"; 
             "Makefile"; 
             "configure"; 
             "src/libtest_oasis_c_dependency_stubs.clib";
             "src/test_oasis_c_dependency.mllib"] @ oasis_ocamlbuild_files,
            [],
            []);

         (* With a documentation that is not built *)
         "data/no-install-doc",
         (fun () ->
            long_test,
            [] @ oasis_ocamlbuild_files,
            [],
            []);
        
         (* Need to create a a parent directory *)
         "data/create-parent-dir",
         (fun () ->
            long_test,
            [] @ oasis_ocamlbuild_files,
            [in_data_dir ["toto/toto/toto.txt"]],
            []);

         "data/bug588",
         (fun () ->
            (fun () ->
               long_test ();
               let cmd = 
                 Printf.sprintf
                   "ocamlfind query bitstring > %s 2>&1"
                   (if Sys.os_type = "Win32" then
                      "NUL"
                    else
                      "/dev/null")
               in
                 skip_if 
                   (Sys.command cmd <> 0)
                   "Cannot find package bitstring"),
            ["libtest.mllib"; "libtest.odocl"] 
            @ 
            (List.filter (( <> ) "_tags") oasis_ocamlbuild_files),
            [],
            []);

         "data/bug619",
         (fun () ->
            long_test,
            oasis_ocamlbuild_files,
            [],
            []);

         "data/bug571",
         (fun () ->
            long_test,
            oasis_ocamlbuild_files,
            [],
            []);

         "data/flag-ccopt",
         (fun () ->
            long_test,
            "cryptokit.mllib" :: oasis_ocamlbuild_files,
            [],
            []);

         "data/bug738",
         (fun () ->
            long_test,
            "src/test.mllib" :: "src/META" :: oasis_ocamlbuild_files,
            [in_ocaml_library "test" ["META"; "foo.cmi"; "test.cma"]],
            []);

         "data/bug982",
         (fun () ->
            (fun () ->
              long_test ();
              skip_if (Sys.os_type = "Win32") "UNIX only test"),
            oasis_ocamlbuild_files,
            [],
            []);

         "data/bug823",
         (fun () ->
            (fun () -> 
               long_test ();
               skip_if (Sys.os_type = "Win32") "UNIX test"),
            oasis_ocamlbuild_files,
            [],
            []);

         "data/bugClib",
         (fun () ->
            long_test,
            "META" :: "mylib.mlpack" :: "libmylib_stubs.clib" ::
            oasis_ocamlbuild_files,
            [in_ocaml_library "mylib"
               ["META"; "dllmylib_stubs.so";
                "foo.ml"; "mylib.cma"; "mylib.cmi";
                "mylib.cmxa"; "mylib.cmxs"; 
                "mylib.a"; "libmylib_stubs.a"]],
            [
              try_installed_library "mylib" ["Mylib.Foo"; "Mylib.Bar"]
            ]);

          "data/bug791",
          (fun () ->
             long_test,
             "src/testA.mllib" :: oasis_ocamlbuild_files,
             [],
             []);

          "../examples/object",
          (fun () ->
             long_test,
             "src1/META" :: "src2/META" :: "src2/packed_modules.mlpack" ::
             oasis_ocamlbuild_files,
             [ in_ocaml_library "single_module"
                 ["META"; "single.o" ; "single.mli"; "single.cmx"; "single.cmo";
                  "single.cmi" ] ;
               in_ocaml_library "packed_modules"
                 ["packed_modules.o"; "packed_modules.cmx";
                  "packed_modules.cmo"; "packed_modules.cmi";
                  "m1.ml"; "m2.mli"; "META"] ],
             []);
       ]
    )
    @
    ["data/bug938">::
     bracket_setup 
       ("data/bug938",
        fun () ->
          (fun () -> skip_if (Sys.os_type = "Win32") "UNIX test"),
          oasis_ocamlbuild_files,
          [],
          [])
       (* Run test *)
       (fun _ ->
          (* Run configure target *)
          assert_run_setup 
            ["-configure"; "--enable-all"; "--disable-over"];
          rm ["setup.data"; exec "setup"; "setup.digest"]);

     "TEMP=a b">::
     bracket
       (fun () ->
          let dn = readlink "a b" in
            mkdir dn;
            dn)
       (fun dn ->
          bracket_setup 
            ("data/bug571", 
             fun () ->
               ignore,
               oasis_ocamlbuild_files,
               [],
               [])
            (fun _ ->
               assert_run_setup 
                 ~extra_env:[if Sys.os_type = "Win32" then
                               "TEMP", dn
                             else
                               "TMPDIR", dn]
                 ["-configure"];
               rm ["setup.data"; exec "setup"; "setup.digest"])
            ())
       (fun dn ->
          rm ~recurse:true [dn]);

     "setup with dev mode (weak)">::
      bracket
        ignore
        (fun () ->
           skip_if (Sys.os_type = "Win32") "UNIX test";
           cp ["data/dev/_oasis.v1"] "data/dev/_oasis";
           bracket_setup
             ~dev:true
             ("data/dev",
              fun () ->
                ignore,
                oasis_ocamlbuild_files,
                [],
                [])
             (* Run test *)
             (fun _ ->
                assert_run_setup ["-all"];
                assert_bool
                  "Library .cma not created."
                  (not (Sys.file_exists "_build/mylib.cma"));
                cp ["_oasis.v2"] "_oasis";
                assert_run_setup ["-all"];
                assert_bool
                  "Library .cma created."
                  (Sys.file_exists "_build/mylib.cma");
                assert_run_setup ["-distclean"];
                rm ["META"; "mylib.mllib"; exec "setup"; "setup.digest"];
                cp ["_oasis.v1"] "_oasis")
             ())
        (fun () ->
           rm ["data/dev/_oasis"]);

    "setup with dev mode (light)">::
    bracket
      ignore
      (fun () ->
           cp ["data/dev/_oasis.v2"] "data/dev/_oasis";
           bracket_setup
             ~dev:true ~dynamic:true
             ("data/dev",
              fun () ->
                ignore,
                ["setup.ml"],
                [],
                [])
             (* Run test *)
             (fun _ ->
                assert_bool
                  "setup.ml is smaller than 2kB"
                  (let chn = open_in "setup.ml" in
                     try 
                       let size = in_channel_length chn in
                         close_in chn;
                         size < 2048 (* 2kB *)
                     with e ->
                       close_in chn;
                       raise e);
                assert_run_setup ["-all"];
                assert_bool
                  "Library .cma created."
                  (Sys.file_exists "_build/mylib.cma");
                assert_run_setup ["-distclean"];
                rm ["META"; "mylib.mllib"; exec "setup"; "setup.digest"])
             ())
      (fun () ->
         rm ["data/dev/_oasis"]);

     "setup with no dev mode">::
     bracket
       ignore
       (fun () ->
          cp ["data/dev/_oasis.v1"] "data/dev/_oasis";
          bracket_setup
            ("data/dev",
             fun () ->
               ignore,
               oasis_ocamlbuild_files,
               [],
               [])
            (* Run test *)
            (fun _ ->
               assert_run_setup ["-all"];
               assert_bool
                 "Library .cma not created."
                 (not (Sys.file_exists "_build/mylib.cma"));
               cp ["_oasis.v2"] "_oasis";
               assert_run_setup ["-all"];
               assert_bool
                 "Library .cma still not created."
                 (not (Sys.file_exists "_build/mylib.cma"));
               assert_run_setup ["-distclean"];
               rm ["META"; "mylib.mllib"; exec "setup"; "setup.digest"];
               cp ["_oasis.v1"] "_oasis")
            ())
       (fun () ->
          rm ["data/dev/_oasis"]);

    "ver0.3">::
    bracket_setup
      ("data/ver0.3",
       fun () ->
         ignore,
         oasis_ocamlbuild_files,
         [],
         [])
      (* Run test *)
      (fun _ ->
         assert_run_setup ["-configure"];
         assert_run_setup ["-test"];
         assert_bool
           "test not run."
           (not (Sys.file_exists "test-done"));
         assert_run_setup ["-doc"];
         assert_bool
           "doc done."
           (Sys.file_exists "doc-done");
         assert_run_setup ["-distclean"];
         rm ["test-done"; "doc-done"];


         assert_run_setup ["-configure"; "--enable-tests"; "--disable-docs"];
         assert_run_setup ["-test"];
         assert_bool
           "test run."
           (Sys.file_exists "test-done");
         assert_run_setup ["-doc"];
         assert_bool
           "doc not done."
           (not (Sys.file_exists "doc-done"));
         assert_run_setup ["-distclean"];
         rm ["test-done"; "doc-done"; exec "setup"; "setup.digest"])
    ]
;;
