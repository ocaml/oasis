#!/usr/bin/ocamlrun ocaml

#use "topfind";;
#require "oasis";;
#require "oasis.base";;
#require "pcre";;
#require "fileutils";;

open OASISMessage
open OASISTypes
open OASISUtils
open FileUtil

let with_tmpdir f = 
  let res =
    Filename.temp_file "oasis-dist-" ".dir"
  in
  let clean () = 
(*     rm ~recurse:true [res] *)
    let _i : int = 
      Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote res))
    in
      ()
  in
    Sys.remove res; 
    FileUtil.mkdir res; 
    try 
      f res;
      clean ()
    with e ->
      clean ();
      raise e

let update_oasis_in_tarball fn topdir = 
  with_tmpdir
    (fun dn ->
       BaseExec.run "tar" ["-C"; dn; "-xzf"; fn];
       BaseExec.run "oasis" ["-C"; Filename.concat dn topdir; "setup"];
       BaseExec.run "tar" ["-C"; dn; "-czf"; fn; topdir])

class virtual vcs = 
object
  method check_uncommited_changes = 
    true

  method check_tag (ver: string) =
    true

  method virtual dist : string -> host_filename -> unit

  method virtual tag : string -> unit
end

class svn ~ctxt =
object 
  inherit vcs

  method check_uncommited_changes =
    match BaseExec.run_read_output "svn" ["status"] with
      | [] ->
          true
      | lst ->
          false

  method dist topdir tarball = 
    with_tmpdir 
      (fun dir ->
         let tgt = 
           Filename.concat dir topdir
         in
         let cur_pwd = 
           pwd ()
         in
           BaseExec.run "svn" ["export"; cur_pwd; tgt];
           BaseExec.run "tar" ["-C"; dir; "-czf"; tarball; topdir])

  method tag ver =
    warning ~ctxt "No tag method"
end

class darcs ~ctxt = 
object
  inherit vcs

  val ctxt = ctxt

  method check_uncommited_changes = 
    let ok = 
      ref false
    in
      (* Check that everything is commited *)
      BaseExec.run 
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

  method check_tag ver = 
      let out = 
        BaseExec.run_read_output "darcs" ["show"; "tags"]
      in
        not (List.mem ver out)

  method dist topdir tarball = 
    (* Create the tarball *)
    BaseExec.run "darcs" ["dist"; "--dist-name"; topdir];
    mv (topdir^".tar.gz") tarball

  method tag ver = 
    BaseExec.run "darcs" ["tag"; ver]
end

class git ~ctxt =
object
  inherit vcs

  method check_uncommited_changes = 
    match BaseExec.run_read_output "git" ["status"; "--porcelain"] with 
      | [] -> 
          true
      | _ ->
          false

  method check_tag ver = 
    let out =
      BaseExec.run_read_output "git" ["tag"]
    in
      not (List.mem ver out)

  method dist topdir tarball = 
    let tarfn = 
      Filename.chop_extension tarball
    in
      BaseExec.run 
        "git" 
        ["archive"; "--prefix";  (Filename.concat topdir "");
         "--format"; "tar"; "HEAD"; "-o"; tarfn];
      BaseExec.run "gzip" [tarfn]

  method tag ver =
    BaseExec.run "git" ["tag"; ver]
end

class no_vcs ~ctxt =
object 
  inherit vcs

  val ctxt = ctxt

  method dist topdir tarball = 
    with_tmpdir 
      (fun dir ->
         let tgt = 
           Filename.concat dir topdir
         in
         let cur_pwd = 
           pwd ()
         in
           BaseExec.run "cp" ["-r"; cur_pwd; tgt];
           begin
             try 
               Sys.chdir tgt;
               BaseExec.run "ocaml" ["setup.ml"; "-distclean"];
               Sys.chdir dir;
               BaseExec.run "tar" ["czf"; tarball; topdir];
               Sys.chdir cur_pwd;
             with e ->
               Sys.chdir cur_pwd;
               raise e
           end)

  method tag ver =
    warning ~ctxt "No tag method"
end

let () = 
  let ctxt = 
    {!OASISContext.default with 
         OASISContext.ignore_plugins = true}
  in
  let pkg = 
    OASISParse.from_file
      ~ctxt
      "_oasis"
  in
  let version = 
    OASISVersion.string_of_version 
      pkg.version
  in

  let topdir = 
    pkg.name^"-"^version
  in

  let tarball = 
    Filename.concat (pwd ()) (topdir^".tar.gz")
  in

  let vcs = 
    let test_dir dn () = 
      test Is_dir dn
    in
      try 
        snd
          (List.find 
             (fun (f, res) -> f ())
             [
               test_dir "_darcs", new darcs ctxt;
               test_dir ".git",   new git ctxt;
               test_dir ".svn",   new svn ctxt;
             ])
      with Not_found ->
        new no_vcs ctxt
  in

    if not vcs#check_uncommited_changes then
      begin
        error ~ctxt "Uncommited changes";
        exit 1
      end;

    (* Create the tarball *)
    vcs#dist topdir tarball;

    (* Run "oasis setup" *)
    update_oasis_in_tarball tarball topdir;

    (* Check that the tarball can build *)
    with_tmpdir 
      (fun dir -> 
         let pwd =
           FileUtil.pwd ()
         in
           (* Uncompress tarball in tmpdir *)
           BaseExec.run "tar" ["xz"; "-C"; dir; "-f"; tarball];

           Sys.chdir dir;
           Sys.chdir topdir;

           try 
             let () = 
               (* Check that build, test, doc run smoothly *)
               BaseExec.run "ocaml" ["setup.ml"; "-all"]
             in

             let () = 
               let bak_files = 
                 (* Check for remaining .bak files *)
                 find (Has_extension "bak") 
                   FilePath.current_dir
                   (fun acc fn -> fn :: acc)
                   []
               in
                 if bak_files <> [] then
                   failwithf
                     "Remaining .bak files: %s"
                     (String.concat ", " bak_files)
             in

               Sys.chdir pwd

           with e ->
             Sys.chdir pwd;
             raise e);

    if vcs#check_tag version then
      begin
        vcs#tag version
      end
    else
      begin
        warning ~ctxt "Version %s already tagged" version
      end;
    
    BaseExec.run 
      ~f_exit_code:
      (fun i -> 
         if i <> 0 then
           warning ~ctxt "Cannot sign '%s' with gpg" tarball
         else
           ())
      "gpg" ["-s"; "-a"; "-b"; tarball]
