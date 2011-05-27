
(** Test the devfiles plugin
   
    @author Sylvain Le Gall
  *)

open TestCommon
open OUnit

(* TODO: move this to OUnit *)
let bracket_tmpdir f = 
  bracket
    (fun () ->
       let dn =
         Filename.temp_file "oasis-db-" ".dir"
       in
         FileUtil.rm [dn];
         FileUtil.mkdir dn;
         dn)
    f
    (fun dn ->
       FileUtil.rm ~recurse:true [dn])

let tests = 
  "DevFiles" >::
  (bracket_tmpdir 
     (fun dn ->
        bracket 
          (fun () ->
             let pwd = FileUtil.pwd () in
               Sys.chdir dn;
               pwd)
          (fun _ ->
             FileUtil.cp 
               [in_data "test-devfiles1.oasis"] 
               "_oasis";
             assert_command 
               (oasis ()) (!oasis_args @ ["setup"]);
             assert_command
               "./configure" ["--prefix=/usr"; "--mandir=/usr/share/man"; 
                              "--infodir=/usr/share/info"; "--datadir=/usr/share";
                              "--sysconfdir=/etc"; "--localstatedir=/var/lib"];)
          (fun pwd ->
             Sys.chdir pwd)
          ()))



