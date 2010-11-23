#!/usr/bin/ocamlrun ocaml

#use "topfind";;
#require "oasis";;
#require "oasis.base";;
#require "pcre";;
#require "fileutils";;

open OASISMessage
open OASISTypes
open FileUtil

let with_tmpdir f = 
  let res =
    Filename.temp_file "oasis-dist-" ".dir"
  in
  let clean () = 
    rm ~recurse:true [res]
  in
    Sys.remove res; 
    FileUtil.mkdir res; 
    try 
      f res;
      clean ()
    with e ->
      clean ();
      raise e

type contxt = OASISContext.t

class virtual vcs = 
object
  method check_dist = 
    true

  method check_uncommited_changes = 
    true

  method check_tag (ver: string) =
    true

  method virtual dist : host_filename -> unit

  method virtual tag : string -> unit
end

class darcs ~ctxt = 
object
  inherit vcs

  val ctxt = ctxt

  method check_dist = 
    (* Check that predist contains "OASIS setup" *)
    let predist_ok =
      ref false
    in
    let () = 
      try 
        let chn = 
          open_in (Filename.concat "_darcs" (Filename.concat "prefs" "prefs"))
        in
          Pcre.foreach_line 
            ~ic:chn
            (fun line ->
               if Pcre.pmatch ~pat:"^predist.*\\boasis\\b.*\\bsetup\\b.*$" line then
                 predist_ok := true);
      with _ ->
        ()
    in
      if not !predist_ok then
        begin
          error ~ctxt 
            "predist doesn't contain 'oasis setup' invocation";
          error ~ctxt
            "Run 'darcs setpref predist \"oasis setup\"'";
          false
        end
      else
        true

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
               error ~ctxt "Uncommited changes"
           | n ->
               error ~ctxt "Unexpected exit code %d" n
        )
        "darcs" ["whatsnew"; "-ls"];
      !ok

  method check_tag ver = 
      let out = 
        BaseExec.run_read_output "darcs" ["show"; "tags"]
      in
        not (List.mem ver out)

  method dist tarname = 
    (* Create the tarball *)
    BaseExec.run "darcs" ["dist"; "--dist-name"; tarname]

  method tag ver = 
    BaseExec.run "darcs" ["tag"; ver]
end

class no_vcs ~ctxt =
object 
  inherit vcs

  val ctxt = ctxt

  method dist tarname = 
    with_tmpdir 
      (fun dir ->
         let tgt = 
           Filename.concat dir tarname
         in
         let cur_pwd = 
           pwd ()
         in
         let tarball =
           Filename.concat cur_pwd (tarname^".tar.gz")
         in
(*
           cp ~recurse:true [cur_pwd] tgt;
 *)
           BaseExec.run "cp" ["-r"; cur_pwd; tgt];
           begin
             try 
               Sys.chdir tgt;
               BaseExec.run "ocaml" ["setup.ml"; "-distclean"];
               Sys.chdir dir;
               BaseExec.run "tar" ["czf"; tarball; tarname];
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

  let tarname = 
    pkg.name^"-"^version
  in

  let tarball = 
    tarname^".tar.gz"
  in

  let vcs = 
    let test_dir dn () = 
      test Is_dir "_darcs"
    in
      try 
        snd
          (List.find 
             (fun (f, res) -> f ())
             [
               test_dir "_darcs", new darcs ctxt;
(*
               test_dir ".git",   Some Git;
               test_dir ".svn",   Some Svn;
 *)
             ])
      with Not_found ->
        new no_vcs ctxt
  in

  let () = 
    if not vcs#check_dist then
      exit 1;

    if not vcs#check_uncommited_changes then
      exit 1;
  in

    (* Create the tarball *)
    vcs#dist tarname;

    (* Check that the tarball can build *)
    with_tmpdir 
      (fun dir -> 
         let pwd =
           FileUtil.pwd ()
         in
           (* Uncompress tarball in tmpdir *)
           BaseExec.run "tar" ["xz"; "-C"; dir; "-f"; tarball];

           Sys.chdir dir;
           Sys.chdir tarname;

           try 
             (* Check that build, test, doc run smoothly *)
             BaseExec.run "ocaml" ["setup.ml"; "-all"];
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
