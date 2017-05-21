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


open OASISMessage
open OASISTypes
open OASISUtils


let run ~ctxt ?f_exit_code prg args =
  OASISExec.run ~ctxt ?f_exit_code prg args


let with_tmpdir f =
  let res =
    Filename.temp_file "oasis-dist-" ".dir"
  in
  let clean () =
    FileUtil.rm ~recurse:true [res]
  in
  Sys.remove res;
  FileUtil.mkdir res;
  try
    f res;
    clean ()
  with e ->
    clean ();
    raise e


let run_oasis_setup_in_tarball ~ctxt fn topdir =
  with_tmpdir
    (fun dn ->
       let () = run ~ctxt "tar" ["-C"; dn; "-xzf"; fn] in
       let dn_topdir = Filename.concat dn topdir in
       let pkg =
         OASISParse.from_file ~ctxt
           (Filename.concat dn_topdir OASISParse.default_oasis_fn)
       in
       let cur_pwd = Sys.getcwd () in
       let () =
         try
           let _chngs: OASISFileTemplate.file_generate_change list =
             Sys.chdir dn_topdir;
             BaseGenerate.generate
               ~ctxt
               ~setup_fn:BaseSetup.default_filename
               ~backup:false
               ~restore:false
               OASISSetupUpdate.NoUpdate
               pkg
           in
           Sys.chdir cur_pwd
         with e ->
           Sys.chdir cur_pwd;
           raise e
       in
       run ~ctxt "tar" ["-C"; dn; "-czf"; fn; topdir])


class virtual vcs =
  object
    method virtual check_uncommited_changes: bool

    method virtual list_tags: string list

    method virtual dist: string -> host_filename -> unit

    method virtual tag: string -> unit
  end


class svn ctxt =
  object
    inherit vcs

    val ctxt = ctxt

    method check_uncommited_changes =
      match OASISExec.run_read_output ~ctxt "svn" ["status"] with
      | [] -> true
      | _ -> false

    method dist topdir tarball =
      with_tmpdir
        (fun dir ->
           let tgt =
             Filename.concat dir topdir
           in
           let cur_pwd = Sys.getcwd () in
           run ~ctxt "svn" ["export"; cur_pwd; tgt];
           run ~ctxt "tar" ["-C"; dir; "-czf"; tarball; topdir])

    method tag _ = warning ~ctxt "No tag method"
    method list_tags = []
  end


(* TODO: check file permissions +x for darcs *)
class darcs ctxt =
  object
    inherit vcs

    val ctxt = ctxt

    method check_uncommited_changes =
      let ok = ref false in
      (* Check that everything is commited *)
      run
        ~ctxt
        ~f_exit_code:
          (function
            | 1 ->
              ok := true
            | 0 ->
              ()
            | n ->
              failwithf
                "Unexpected exit code %d" n
          )
        "darcs" ["whatsnew"; "-ls"];
      !ok

    method list_tags =
      OASISExec.run_read_output ~ctxt
        "darcs" ["show"; "tags"]

    method dist topdir tarball =
      (* Create the tarball *)
      run ~ctxt "darcs" ["dist"; "--dist-name"; topdir];
      Sys.rename (topdir^".tar.gz") tarball

    method tag ver =
      run ~ctxt "darcs" ["tag"; ver]
  end


class git ctxt =
  object
    inherit vcs

    val ctxt = ctxt

    method check_uncommited_changes =
      match OASISExec.run_read_output ~ctxt
              "git" ["status"; "--porcelain"] with
        | [] -> true
        | _  -> false

    method list_tags =
      OASISExec.run_read_output ~ctxt "git" ["tag"]

    method dist topdir tarball =
      let tarfn =
        Filename.chop_extension tarball
      in
      run ~ctxt
        "git"
        ["archive"; "--prefix";  (Filename.concat topdir "");
         "--format"; "tar"; "HEAD"; "-o"; tarfn];
      run ~ctxt "gzip" [tarfn]

    method tag ver = run ~ctxt "git" ["tag"; ver]
  end


class no_vcs ctxt =
  object
    inherit vcs

    val ctxt = ctxt

    method dist topdir tarball =
      with_tmpdir
        (fun dir ->
           let tgt =
             Filename.concat dir topdir
           in
           let cur_pwd = Sys.getcwd () in
           OASISFileUtil.cp ~ctxt ~recurse:true cur_pwd tgt;
           begin
             try
               Sys.chdir tgt;
               run ~ctxt "ocaml" ["setup.ml"; "-distclean"];
               Sys.chdir dir;
               run ~ctxt "tar" ["czf"; tarball; topdir];
               Sys.chdir cur_pwd;
             with e ->
               Sys.chdir cur_pwd;
               raise e
           end)

    method tag _ = warning ~ctxt "No tag method"
    method check_uncommited_changes = false
    method list_tags = []
  end


let () =
  let () = OASISBuiltinPlugins.init () in
  let build = ref true in
  let tag = ref true in
  let sign = ref true in
  let ignore_changes = ref false in
  let () =
    Arg.parse
      [
        "-no-build",
        Arg.Clear build,
        " Don't try to build the resulting tarball.";

        "-no-tag",
        Arg.Clear tag,
        " Don't tag the result.";

        "-no-sign",
        Arg.Clear sign,
        " Don't sign the result.";

        "-ignore_changes",
        Arg.Set ignore_changes,
        " Ignore local changes.";
      ]
      (fun s -> failwith (Printf.sprintf "Don't know what to do with %S" s))
      "oasis-dist: build tarball out of oasis enabled sources."
  in
  let ctxt =
    {!OASISContext.default with
       OASISContext.ignore_plugins = true}
  in
  let pkg =
    OASISParse.from_file
      ~ctxt
      OASISParse.default_oasis_fn
  in

  let topdir =
    pkg.name^"-"^(OASISVersion.string_of_version pkg.version)
  in

  let tarball =
    Filename.concat (Sys.getcwd ()) (topdir^".tar.gz")
  in

  let vcs =
    let test_dir dn () =
      Sys.file_exists dn && Sys.is_directory dn
    in
    try
      snd
        (List.find
           (fun (f, _) -> f ())
           [
             test_dir "_darcs", new darcs ctxt;
             test_dir ".git",   new git ctxt;
             test_dir ".svn",   new svn ctxt;
           ])
    with Not_found ->
      new no_vcs ctxt
  in

  if not !ignore_changes && not vcs#check_uncommited_changes then
    begin
      error ~ctxt "Uncommited changes";
      exit 1
    end;

  (* Create the tarball *)
  vcs#dist topdir tarball;

  (* Run "oasis setup" *)
  run_oasis_setup_in_tarball ~ctxt tarball topdir;

  (* Check that the tarball can build *)
  with_tmpdir
    (fun dir ->
       let pwd = Sys.getcwd () in
       (* Uncompress tarball in tmpdir *)
       run ~ctxt "tar" ["xz"; "-C"; dir; "-f"; tarball];

       Sys.chdir dir;
       Sys.chdir topdir;

       try
         let () =
           if Sys.file_exists "setup.data" then
             failwith
               "Remaining 'setup.data' file.";
           if Sys.file_exists "configure" &&
              not (FileUtil.test FileUtil.Is_exec "configure") then
             failwith
               "'configure' is not executable."
         in


         let () =
           if !build then
             (* Check that build, test, doc run smoothly *)
             run ~ctxt "ocaml" ["setup.ml"; "-all"]
         in

         let () =
           let bak_files =
             (* Check for remaining .bak files *)
             FileUtil.find (FileUtil.Has_extension "bak")
               Filename.current_dir_name
               (fun acc fn -> fn :: acc)
               []
           in
           if bak_files <> [] then
             failwithf
               "Remaining .bak files: %s."
               (String.concat ", " bak_files)
         in

         Sys.chdir pwd

       with e ->
         Sys.chdir pwd;
         raise e);

  if !tag then
    begin
      let tags =
        List.sort
          OASISVersion.version_compare
          (List.rev_map
             OASISVersion.version_of_string
             vcs#list_tags)
      in
      let ver_str =
        OASISVersion.string_of_version pkg.version
      in
      match tags with
        | hd :: _ ->
          begin
            let cmp =
              OASISVersion.version_compare hd pkg.version
            in
            if List.mem pkg.version tags then
              begin
                warning ~ctxt "Version %s already tagged" ver_str
              end
            else if cmp > 0 then
              begin
                warning ~ctxt
                  "Version %s is smaller than already tagged version %s"
                  ver_str (OASISVersion.string_of_version hd);
                vcs#tag ver_str
              end
            else
              begin
                vcs#tag ver_str
              end
          end
        | _ ->
          vcs#tag ver_str
    end;

  if !sign then
    run
      ~ctxt
      ~f_exit_code:
        (fun i ->
           if i <> 0 then
             warning ~ctxt "Cannot sign '%s' with gpg" tarball
           else
             ())
      "gpg" ["-s"; "-a"; "-b"; tarball]
