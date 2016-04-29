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


(** Utilities to run full OASIS use cases
    @author Sylvain Le Gall
  *)


open FileUtil
open OUnit2
open TestCommon


type filename = FilePath.filename


let exec fn =
  if Sys.os_type = "Win32" then
    fn^".exe"
  else
    fn


(* Print a short version of the filename *)
let fn_printer ~root fn = FilePath.make_relative root fn


module SetFileElement =
struct
  type t = filename
  let compare =
    if Sys.os_type = "Win32" then
      (* Win32 FS is case insensitive *)
      OASISUtils.compare_csl
    else
      String.compare

  let pp_printer = Format.pp_print_string

  let pp_print_sep = OUnitDiff.pp_comma_separator
end


module SetFile =
struct
  include OASISUtils.SetExt.Make(SetFileElement)
  module Diff = OUnitDiff.SetMake(SetFileElement)

  let assert_equal ?msg ~root exp act =
    Diff.assert_equal ?msg
      (Diff.of_list (List.rev_map (fn_printer ~root) (elements exp)))
      (Diff.of_list (List.rev_map (fn_printer ~root) (elements act)))
end


module SetFileDigestElement =
struct
  type t = filename * Digest.t

  let compare (f1, d1) (f2, d2) =
    match SetFileElement.compare f1 f2 with
      | 0 -> String.compare d1 d2
      | n -> n

  let pp_printer frmtr (filename, digest) =
    Format.fprintf frmtr "(%S, %s)" filename (Digest.to_hex digest)

  let pp_print_sep = OUnitDiff.pp_comma_separator
end


module SetFileDigest =
struct
  include OASISUtils.SetExt.Make(SetFileDigestElement)
  module Diff = OUnitDiff.SetMake(SetFileDigestElement)

  let assert_equal ?msg ~root exp act =
    let convert lst =
      Diff.of_list
        (List.rev_map
           (fun (fn, digest) ->
              fn_printer ~root fn, digest)
           (elements exp))
    in
    Diff.assert_equal ?msg (convert exp) (convert act)
end


let setup_ml = BaseSetup.default_filename


(* List all files in directory. *)
let all_files dir =
  find Is_file dir (fun st fn -> SetFile.add fn st) SetFile.empty


(* Create a set of file/digest of the current directory. *)
let all_file_digests dn =
  SetFile.fold
    (fun fn st ->
       SetFileDigest.add (fn, Digest.file fn) st)
    (all_files dn)
    SetFileDigest.empty


(* Verify that generated files follow some style rules. *)
let check_file_style test_ctxt fn =
  let chn = open_in fn in
  let line_number = ref 0 in
  try
    while true do
      let line = input_line chn in
      incr line_number;
      non_fatal test_ctxt
        (fun test_ctxt ->
           let strlen = String.length line in
           if strlen > 0 && line.[strlen - 1] = ' ' then
             assert_failure
               (Printf.sprintf
                  "Found a blank at the end of line in file '%s' line %d: %S"
                  (Filename.basename fn) !line_number line));
    done
  with End_of_file ->
    close_in chn


(* Find and test the style of all files in a directory. *)
let check_all_files_style test_ctxt dn =
  let ocamlmod_version =
    try
      let buff = Buffer.create 10 in
      OUnit2.assert_command ~ctxt:test_ctxt
        ~foutput:(fun strm -> Stream.iter (Buffer.add_char buff) strm)
        (TestCommon.ocamlmod_exec test_ctxt) ["-version"];
      Buffer.contents buff
    with _ ->
      "0.0.6"
  in
    logf test_ctxt `Info "Using ocamlmod version %s." ocamlmod_version;
    if OASISVersion.version_compare_string ocamlmod_version "0.0.7" >= 0 then
      FileUtil.find Is_file dn
        (fun () fn -> check_file_style test_ctxt fn)
        ()
    else
      logf test_ctxt `Warning
        "Skipping style check because using ocamlmod version %s (< 0.0.7)."
        ocamlmod_version


type t =
    {
      is_native: bool;
      native_dynlink: bool;
      src_dir: filename;
      build_dir: filename;
      ocaml_lib_dir: filename;
      bin_dir: filename;
      lib_dir: filename;
      data_dir: filename;
      doc_dir: filename;
      html_dir: filename;
      precompile_dir: filename;
      pristine: SetFileDigest.t;
      ocaml_version: string;
      mutable generated_files: SetFile.t;
      mutable installed_files: SetFile.t;
      mutable setup_ml_precompiled:
        [`Not_tried|`Not_possible|`Done_for of Digest.t];
    }


(* Create tree structure for a test project and copy it there. *)
let setup_test_directories test_ctxt ~is_native ~native_dynlink dn =
  (* Create a temporary directory. *)
  let tmpdir = bracket_tmpdir test_ctxt in

  (* Copy sources in this temporary directory. *)
  let src_dir =
    OASISFileUtil.cp ~ctxt:oasis_ctxt ~recurse:true dn tmpdir;
    Filename.concat tmpdir (Filename.basename dn)
  in

  (* Directory where we store precompiled setup.ml. *)
  let precompile_dir =
    let dn = Filename.concat tmpdir "precompile" in
      mkdir dn;
      dn
  in

  (* Create the build_dir. *)
  let build_dir = Filename.concat tmpdir "build" in

  (* Create a directory in build_dir and return its name *)
  let mkdir_return fn_parts =
    let fn = FilePath.make_filename (build_dir :: fn_parts) in
      mkdir ~parent:true fn;
      fn
  in

  (* Evaluate the ocaml version, in the current environment. *)
  let ocaml_version =
    let buff = Buffer.create 10 in
    OUnit2.assert_command ~ctxt:test_ctxt
      ~foutput:(Stream.iter
                  (function '\n' -> () | c -> Buffer.add_char buff c))
      "ocamlc" ["-version"];
    Buffer.contents buff
  in
    {
      is_native = is_native;
      native_dynlink = native_dynlink;
      src_dir = src_dir;
      build_dir = build_dir;
      ocaml_lib_dir = mkdir_return ["lib"; "ocaml"];
      bin_dir = mkdir_return ["bin"];
      lib_dir = mkdir_return ["lib"];
      data_dir = mkdir_return ["share"];
      doc_dir = mkdir_return ["share"; "doc"];
      html_dir = mkdir_return ["share"; "doc"; "html"];
      precompile_dir = precompile_dir;
      pristine = all_file_digests src_dir;
      ocaml_version = ocaml_version;
      generated_files = SetFile.empty;
      installed_files = SetFile.empty;
      setup_ml_precompiled = `Not_tried;
    }


(* Locate the given [fn] in src_dir. *)
let in_src_dir t fn = Filename.concat t.src_dir fn


(* Precompile setup.ml to speedup the tests, if possible. *)
let rec precompile_setup_ml test_ctxt t =
  let setup_exe =
    Filename.concat t.precompile_dir (Filename.chop_extension setup_ml)
  in
  let full_setup_ml = in_src_dir t setup_ml in

  let can_compile () =
    let chn = open_in full_setup_ml in
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

  let compile () =
    let timer = timer_start "precompile_setup_ml" in
    let exit_code =
      FileUtil.cp ~force:FileUtil.Force [full_setup_ml] t.precompile_dir;
      Sys.command ("ocamlfind ocamlc -o "^setup_exe^" "
                   ^(Filename.concat t.precompile_dir setup_ml))
    in
    timer_stop test_ctxt timer;
    if exit_code = 0 then begin
      (* Compilation succeed, update the digest *)
      logf test_ctxt `Info "Compilation of setup.ml succeeds.";
      `Done_for (Digest.file full_setup_ml)
    end else begin
      logf test_ctxt `Warning "Compilation of setup.ml doesn't succeed.";
      `Not_possible
    end
  in
    match t.setup_ml_precompiled with
      | `Not_tried ->
          if can_compile () then begin
            t.setup_ml_precompiled <- compile ()
          end else begin
            logf test_ctxt `Info "Compilation of setup.ml not possible.";
            t.setup_ml_precompiled <- `Not_possible
          end;
          precompile_setup_ml test_ctxt t

      | `Not_possible ->
          None

      | `Done_for digest ->
          if (Digest.file full_setup_ml) = digest then begin
            Some (exec setup_exe)
          end else begin
            t.setup_ml_precompiled <- compile ();
            precompile_setup_ml test_ctxt t
          end


(* Run a command after setting everything to run using binaries and libraries
   as generated by "ocaml setup.ml -install" of a test project.
 *)
let assert_command_with_ocaml_env
      ?(extra_env=[])
      ?exit_code
      ?chdir
      ?check_output
      test_ctxt t cmd args =
  (* Libraries located inside the test directory *)
  let local_lib_paths =
    find Is_dir t.lib_dir (fun acc fn -> fn :: acc)
      (find Is_dir t.ocaml_lib_dir (fun acc fn -> fn :: acc) [])
  in
  let env_paths =
    try FilePath.path_of_string (Unix.getenv "PATH") with Not_found -> []
  in
  let paths, extra_paths =
    if Sys.os_type = "Win32" then begin
      (t.bin_dir :: (local_lib_paths @ env_paths)), []
    end else begin
      let paths =
        t.bin_dir :: env_paths
      in
      let ld_library_paths =
        local_lib_paths @
        (try
           FilePath.path_of_string (Unix.getenv "LD_LIBRARY_PATH")
         with Not_found ->
           [])
      in
        paths, ["LD_LIBRARY_PATH", ld_library_paths]
    end
  in
  let real_cmd =
    try
      if not (Sys.file_exists cmd) then
        which ~path:paths cmd
      else
        cmd
    with Not_found ->
      assert_failure
        (Printf.sprintf
           "Command '%s' cannot be found in %s."
           cmd
           (String.concat ";" paths))
  in
  (* Add a path to a path like environment varialbe. *)
  let add_path nm dir =
    nm,
    try
      FilePath.string_of_path
        ((FilePath.path_of_string (Sys.getenv nm)) @ [dir])
    with Not_found ->
      dir
  in
    assert_command
      ~ctxt:test_ctxt
      ?exit_code
      ?chdir
      ?check_output
      ~extra_env:((add_path "OCAMLPATH" t.ocaml_lib_dir)
                  ::
                  (List.map
                     (fun (v, lst) ->
                        v, FilePath.string_of_path lst)
                     (("PATH", paths) :: extra_paths))
                  @ extra_env)
      real_cmd
      args

(* Run setup.ml *)
let run_ocaml_setup_ml
      ?(with_ocaml_env=false)
      ?exit_code
      ?check_output
      ?(extra_env=[])
      test_ctxt t args =
  (* Speed up for testing, compile setup.ml *)
  let timer = timer_start ("run_ocaml_setup_ml "^(List.hd args)) in
  let toplevel_path =
    try Sys.getenv "OCAML_TOPLEVEL_PATH" with Not_found -> ""
  in
  let extra_env =
    ("OCAMLFIND_DESTDIR", t.ocaml_lib_dir)
    :: ("OCAMLFIND_LDCONF", "ignore")
    :: ("OCAML_TOPLEVEL_PATH", toplevel_path)
    :: extra_env
  in
  let cmd, args =
    match precompile_setup_ml test_ctxt t with
      | Some setup_exe -> setup_exe, ("-info" :: "-debug" :: args)
      | None ->
          "ocaml", ((in_src_dir t setup_ml) :: "-info" :: "-debug" :: args)
  in
    if with_ocaml_env then
      assert_command_with_ocaml_env ?exit_code ?check_output ~extra_env
        ~chdir:t.src_dir test_ctxt t cmd args
    else
      assert_command ~ctxt:test_ctxt ?exit_code ?check_output ~extra_env
        ~chdir:t.src_dir cmd args;
    timer_stop test_ctxt timer


(* Try to run an installed executable *)
let try_installed_exec ?exit_code test_ctxt t cmd args =
  assert_command_with_ocaml_env test_ctxt ?exit_code t cmd args

(* Compile with the given package. *)
let assert_compile test_ctxt t pkg cmd args =
  assert_command_with_ocaml_env test_ctxt t
    "ocamlfind" (cmd :: "-package" :: pkg :: args)

(* Try to run an installed library. *)
let try_installed_library test_ctxt t pkg modules =
  (* Create a file that contains every modules *)
  let srcdir = bracket_tmpdir test_ctxt in
  let fn = FilePath.concat srcdir "testZZZ.ml" in
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

  let exec_byte = FilePath.replace_extension fn "byte" in
  let exec_native = FilePath.replace_extension fn "native" in
    (* Library + bytecode compilation *)
    assert_compile test_ctxt t pkg
      "ocamlc" ["-a"; "-o"; FilePath.replace_extension fn "cma"; fn];

    (* Program + bytecode compilation *)
    assert_compile test_ctxt t pkg
      "ocamlc" ["-o"; exec_byte; "-linkpkg"; fn];

    if t.is_native then begin
      (* Library + native compilation *)
      assert_compile test_ctxt t pkg
        "ocamlopt" ["-a"; "-o"; FilePath.replace_extension fn "cmxa"; fn];

      (* Program + native compilation *)
      assert_compile test_ctxt t pkg
        "ocamlopt" ["-o"; exec_native; "-linkpkg"; fn];
    end


let contains_string fn str =
  let rex = Pcre.regexp (Pcre.quote str) in
  let has_string = ref false in
  let chn = open_in fn in
    Pcre.foreach_line
      ~ic:chn
      (fun l ->
         if not !has_string then
           has_string := Pcre.pmatch ~rex l);
    close_in chn;
    !has_string


(* Files generated when ocamlbuild buildsys is used *)
let oasis_ocamlbuild_files = ["myocamlbuild.ml"; "_tags"]


(* Add files to the list of generated files. *)
let register_generated_files t lst =
  List.iter
    (fun fn ->
       t.generated_files <- SetFile.add (in_src_dir t fn) t.generated_files)
    lst


(* Check presence of generated files. *)
let check_generated_files t =
  let expected_files =
    SetFileDigest.fold
      (fun (fn, _) st -> SetFile.add fn st)
      t.pristine t.generated_files
  in
  (* Check generated files *)
  SetFile.assert_equal
    ~msg:"Generated files"
    ~root:t.src_dir
    expected_files
    (all_files t.src_dir)


(* Remove generated files and check that we are back to pristine. *)
let back_to_pristine t =
  (* Remove generated files. *)
  rm (SetFile.elements t.generated_files);
  (* Check that we are back to pristine.  *)
  SetFileDigest.assert_equal
    ~msg:"Source directory back to pristine"
    ~root:t.src_dir
    t.pristine
    (all_file_digests t.src_dir)


type installed_files =
  | InstalledLibrary of filename list
  | InstalledOCamlLibrary of string * filename list (* lib * files *)
  | InstalledHTML of string * filename list (* lib * files *)
  | InstalledAPIRef of string * string list (* lib * module name *)
  | InstalledData of filename list
  | InstalledBin of filename list


(* Register a set of files expected to be built. *)
let register_installed_files test_ctxt t installed_files_lst =
  let rec file_list =
    function
      | InstalledLibrary fn_lst ->
         List.rev_map (FilePath.concat t.lib_dir) fn_lst
      | InstalledOCamlLibrary (lib, fn_lst) ->
         List.rev_map
           (fun fn -> FilePath.make_filename [t.ocaml_lib_dir; lib; fn])
           fn_lst
      | InstalledHTML (lib, fn_lst) ->
         List.rev_map
           (fun fn -> FilePath.make_filename [t.html_dir; lib; fn])
           fn_lst
      | InstalledAPIRef (lib, modules) ->
          let files =
            List.rev_append
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
                    modules))
          in
            file_list (InstalledHTML (lib, files))
      | InstalledData fn_lst ->
         List.rev_map (FilePath.concat t.data_dir) fn_lst
      | InstalledBin fn_lst ->
         List.rev_map (fun fn -> exec (FilePath.concat t.bin_dir fn)) fn_lst
  in
  (** Filter out and transform an expected file list to be compatible with the
    * target platform.
    *)
  let adapt_files_to_platform test_ctxt lst =
    let is_win32 = Sys.os_type = "Win32" in
    List.fold_left
      (fun acc fn ->
         let ext =
           try FilePath.get_extension fn with Not_found -> ""
         in
         match ext with
           | "cmx" | "cmxa" | "o" when not t.is_native ->
               acc
           | "cmxs" when not t.native_dynlink ->
               acc
           | "annot" when
               OASISVersion.version_compare_string t.ocaml_version "3.11" < 0 ->
               acc
           | "cmt" | "cmti" when
               OASISVersion.version_compare_string t.ocaml_version "4.00" < 0 ->
               acc
           | "a" ->
               let fn =
                 if is_win32 then FilePath.replace_extension fn "lib" else fn
               in
                 if (* library matching the .cmxa *)
                   t.is_native ||
                    (* stubs library *)
                   OASISString.starts_with ~what:"lib"
                      (Filename.basename fn) then
                   fn :: acc
                 else
                   (* no .a matching bytecode only library. *)
                   acc
           | "so" when is_win32 ->
               (FilePath.replace_extension fn ".dll") :: acc
           | _ ->
               fn :: acc)
      [] lst
  in
    t.installed_files <-
    List.fold_left
      (fun set fn -> SetFile.add fn set)
      t.installed_files
      (adapt_files_to_platform test_ctxt
         (List.flatten (List.rev_map file_list installed_files_lst)))


(* Check that we have installed everything as expected. *)
let check_installed_files test_ctxt t id =
   SetFile.assert_equal
     ~msg:(Printf.sprintf "Installed files (%s)" id)
     ~root:t.build_dir
     t.installed_files
     (all_files t.build_dir)


(* Check build_dir is empty. *)
let check_nothing_installed test_ctxt t id =
  SetFile.assert_equal
    ~msg:(Printf.sprintf "Build directory is empty after uninstall (%s)" id)
    ~root:t.build_dir
    SetFile.empty
    (all_files t.build_dir)


(* Extract ocamlbuild flags and check that they are correct. *)
let check_myocamlbuild_ml test_ctxt t =
  if Sys.file_exists (in_src_dir t "myocamlbuild.ml") then begin
    let timer = timer_start "check_myocamlbuild_ml" in
    let () = dbug_file_content test_ctxt (in_src_dir t "myocamlbuild.ml") in
    let documentation_output =
      let buf = Buffer.create 16000 in
      OUnit2.assert_command
        ~chdir:t.src_dir
        ~ctxt:test_ctxt
        ~foutput:(Stream.iter (Buffer.add_char buf))
        "ocamlbuild" ["-documentation"];
      Buffer.contents buf
    in
    let lst = OASISString.split_newline documentation_output in
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
                | '_' | '.' | ':' | '-' | '+' | '(' | ')' | '@' ->
                    ()
                | c ->
                    assert_failure
                      (Printf.sprintf
                         "flag %S contains %C which is illegal."
                         flag c))
             flag)
        !rst;
      timer_stop test_ctxt timer
  end


(* If a _tags file exists, try to test its content. *)
let check_tags test_ctxt t =
  if Sys.file_exists (in_src_dir t "_tags") then begin
    let () =
     dbug_file_content test_ctxt (in_src_dir t "_tags")
    in
    let _, lst =
      List.fold_right
        (fun line (lineno, lst) -> lineno + 1, (lineno, line) :: lst)
        (OASISString.nsplit (file_content (in_src_dir t "_tags")) '\n')
        (1, [])
    in
      List.iter
        (fun (lineno, line) ->
           let assert_file_exists fn =
             if not (Sys.file_exists (in_src_dir t fn)) then
               assert_failure
                 (Printf.sprintf
                    "file '%s' doesn't exist in _tags file line %d (%S)"
                    fn lineno line)
           in
           try
             let _ = "(*" in
             let substr = Pcre.exec ~pat:"^\\s*\"(.*)\"\\s*:" line in
             let fn = Pcre.get_substring substr 1 in
               if List.mem fn
                    [".git"; ".bzr"; ".hg"; "_darcs"] ||
                  List.mem (FilePath.get_extension fn)
                    ["byte"; "native"; "lib"; "dll"; "a"; "so"; "cmxs"] then
                 ()
               else if FilePath.get_extension fn = "cmx" then begin
                 assert_file_exists (FilePath.replace_extension fn "ml")
               end else begin
                 assert_file_exists fn
               end
           with Not_found ->
             (* TODO: handle ocamlbuild wildcard *)
             ())
        lst
  end


(* Run oasis setup and fix generated files accordingly. *)
let oasis_setup ?(dev=false) ?(dynamic=false) test_ctxt t =
  let () =
    let pkg =
      OASISParse.from_file
        ~ctxt:oasis_ctxt
        (in_src_dir t OASISParse.default_oasis_fn)
    in
    match pkg.OASISTypes.ocaml_version with
      | Some ver_cmp ->
          skip_if
            (not
               (OASISVersion.comparator_apply
                  (OASISVersion.version_of_string t.ocaml_version)
                  ver_cmp))
            (Printf.sprintf
               "Need ocaml version %s."
               (OASISVersion.string_of_comparator ver_cmp))
      | None ->
          ()
  in
  let timer = timer_start "oasis_setup" in
    (* Create build system using OASIS *)
    assert_oasis_cli
      ~ctxt:test_ctxt
      ~chdir:t.src_dir
      ("setup" ::
       (if dev then
          ["-real-oasis"; "-setup-update";
           if dynamic then "dynamic" else "weak"]
        else
          []));
    timer_stop test_ctxt timer;

    register_generated_files t [setup_ml];

    (* Fix #require in dynamic *)
    if dynamic then begin
      let load lst =
        let cma =
          FilePath.make_filename
            ([FileUtil.pwd (); ".."; "_build"; "src"] @ lst)
        in
        Printf.sprintf "#load %S;;\n#directory %S;;" cma (Filename.dirname cma)
      in
      let orig_lst =
        OASISString.nsplit
          (file_content (in_src_dir t setup_ml))
          '\n'
      in
      let fixed_lst =
        List.fold_left
          (fun fixed_lst line ->
             match line with
             | "#require \"oasis.dynrun\";;" ->
                 List.rev_append
                   [
                     "#require \"unix\";;";
                     "#require \"odn\";;";
                     "#require \"ocamlbuild\";;";
                     (* TODO: problem with gettext when using --enable-gettext.
                      *)
                     "#require \"gettext.base\";;";
                     load ["oasis"; "oasis.cma"];
                     load ["base"; "base.cma"];
                     load ["builtin-plugins.cma"];
                     load ["dynrun"; "dynrun.cma"];
                   ] fixed_lst
             | "    let _str : string = Findlib.package_directory \
                 \"oasis.dynrun\" in" ->
                 (* For dynrun_for_release. *)
                 "    let _str : string = Findlib.package_directory \
                 \"unix\" in" :: fixed_lst
             | line -> line :: fixed_lst)
          [] orig_lst
      in
      let chn = open_out (in_src_dir t setup_ml) in
        List.iter
          (fun line ->
             output_string chn line;
             output_char chn '\n')
          (List.rev fixed_lst);
        close_out chn;
        dbug_file_content test_ctxt (in_src_dir t setup_ml)
    end;

    check_all_files_style test_ctxt t.src_dir


let standard_checks test_ctxt t =
  let timer = timer_start "standard_checks" in
    check_generated_files t;
    check_tags test_ctxt t;
    timer_stop test_ctxt timer


let standard_test test_ctxt t =
  (* Standard checks. *)
  standard_checks test_ctxt t;

  (* Quick test. *)
  run_ocaml_setup_ml test_ctxt t
    ["-all"; "--"; "--override"; "is_native"; string_of_bool t.is_native];
  check_myocamlbuild_ml test_ctxt t;

  (* Distclean. *)
  run_ocaml_setup_ml test_ctxt t ["-distclean"];
  check_generated_files t;

  (* Run configure target *)
  run_ocaml_setup_ml test_ctxt t
      ["-configure"; "--prefix";  t.build_dir; "--docdir";  t.doc_dir;
       "--htmldir"; t.html_dir;
       "--override"; "is_native"; string_of_bool t.is_native];
  assert_bool "File 'setup.data' has been created"
    (Sys.file_exists (in_src_dir t "setup.data"));

  (* Run build target *)
  run_ocaml_setup_ml test_ctxt t ["-build"];

  (* Clean *)
  run_ocaml_setup_ml test_ctxt t ["-clean"];

  (* Run build target *)
  run_ocaml_setup_ml test_ctxt t ["-build"];

  (* Run test target *)
  run_ocaml_setup_ml test_ctxt t ["-test"];

  (* Run documentation target *)
  run_ocaml_setup_ml test_ctxt t ["-doc"];

  (* 1st install *)
  run_ocaml_setup_ml test_ctxt t ["-install"];
  check_installed_files test_ctxt t "1st install";
  run_ocaml_setup_ml test_ctxt t ["-uninstall"];
  check_nothing_installed test_ctxt t "1st uninstall";

  (* 2nd install *)
  run_ocaml_setup_ml test_ctxt t ["-install"];
  check_installed_files test_ctxt t "2nd install";
  run_ocaml_setup_ml test_ctxt t ["-uninstall"];
  check_nothing_installed test_ctxt t "2nd uninstall";

  (* Run install/uninstall target with destdir *)
  if Sys.os_type <> "Win32" then begin
    (* Prepending something at the beginning of a Win32 path
     * doesn't work because it will create a filename like:
     * c:\a\b\c:\c, which is illegal
     * TODO: find a solution for DESTDIR on Win32
     *)
    let destdir = bracket_tmpdir test_ctxt in
    let () =
      List.iter
        (fun lst ->
           mkdir ~parent:true (FilePath.make_filename (destdir :: lst)))
        [
          ["bin"];
          ["lib"; "ocaml"];
          ["share"; "doc"; "html"]
        ]
    in
    let t =
      {t with
           (* This will change OCAMLPATH as well when running setup.ml. *)
           ocaml_lib_dir = FilePath.make_filename [destdir; "lib"; "ocaml"]}
    in
      run_ocaml_setup_ml test_ctxt t
        ~extra_env:["destdir", destdir] ["-install"];

      assert_equal
        ~msg:"Same number of files installed with destdir and without."
        ~printer:string_of_int
        (SetFile.cardinal t.installed_files)
        (SetFile.cardinal (all_files destdir))
  end;

  (* 3rd install *)
  run_ocaml_setup_ml test_ctxt t ["-reinstall"];
  check_installed_files test_ctxt t "3rd install"
  (* TODO: auto-test installed libraries. *)
