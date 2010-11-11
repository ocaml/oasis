#!/usr/bin/ocamlrun ocaml

#use "topfind";;
#require "oasis";;
#require "oasis.base";;
#require "pcre";;
#require "fileutils";;

open OASISMessage
open OASISTypes

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

  let with_tmpdir f = 
    let res =
      Filename.temp_file "oasis-dist-" ".dir"
    in
      Sys.remove res; 
      FileUtil.mkdir res; 
      f res;
      FileUtil.rm ~recurse:true [res]
  in

  let () = 
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
          exit 1
        end
  in

    (* Check that everything is commited *)
    BaseExec.run 
      ~f_exit_code:
      (function 
         | 1 -> 
             ()
         | 0 ->
             error ~ctxt "Uncommited changes"
         | n ->
             error ~ctxt "Unexpected exit code %d" n)
      "darcs" ["whatsnew"; "-ls"];

    (* Create the tarball *)
    BaseExec.run "darcs" ["dist"; "--dist-name"; tarname];

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

    begin
      (* Check that the tag doesn't exist *)
      let out = 
        BaseExec.run_read_output "darcs" ["show"; "tags"]
      in
        if List.mem version out then
          warning ~ctxt "Version %s already tagged" version
        else
          BaseExec.run "darcs" ["tag"; version]
    end;
    
    BaseExec.run 
      ~f_exit_code:
      (fun i -> 
         if i <> 0 then
           warning ~ctxt "Cannot sign '%s' with gpg" tarball
         else
           ())
      "gpg" ["-s"; "-a"; "-b"; tarball]
