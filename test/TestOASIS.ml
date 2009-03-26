
(** Tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open Oasis;;

let tests ctxt =

  (* Convert environment to string *)
  let string_of_env env =
    String.concat "; "
      (
        (List.map (fun (nm, b)   -> Printf.sprintf "%s = %b" nm b) env.flags)
        @
        (List.map (fun (nm, str) -> Printf.sprintf "%s = '%s'" nm str) env.tests)
      )
  in

  (* Check flag equality *)
  let assert_flag nm lst =
    try
      let _ = 
        List.find 
          (fun (flg, _) -> nm = flg) 
          lst 
      in
        ()
    with Not_found ->
      assert_failure 
        (Printf.sprintf 
           "No flag '%s' defined"
           nm)
  in

  (* Check that at least one alternative doesn't raise an exception *)
  let assert_alternative msg lst e =
    let found_one =
      List.fold_left
        (fun r t ->
           if not r then
             (
               try
                 t e; true
               with _ ->
                 false
             )
           else
             r)
        false
        lst
    in
      if not found_one then
        assert_failure msg
  in

  let test_of_vector (fn, test) = 
    fn >::
    (fun () ->
       let fn =
         in_data fn
       in
       let ast =
         if ctxt.dbug then
           OasisTools.parse_file 
             ~fstream:OasisTools.stream_debugger 
             fn
         else
           OasisTools.parse_file
             fn
       in
       let (env, flags) =
         create fn ast  
       in
       let oasis =
         if ctxt.dbug then
           prerr_endline (string_of_env env);
         oasis (ast, env, flags)
       in
         test env flags oasis)
  in

    "OASIS" >:::
    (List.map test_of_vector 
       [
         "test1.oasis",
         (fun env flags oasis ->
            assert_flag "devmod" flags;
            assert_alternative
              "At least one of ostest, linuxtest64 and linuxtest32 is defined"
              (List.map
                 (fun nm -> (fun () -> assert_flag nm flags))
                 [
                   "ostest";
                   "linuxtest64";
                   "linuxtest32";
                 ])
              ());

         "test2.oasis",
         (fun env flags oasis ->
            ());
       ]
    )
;;

let () = 
  let tmpfiles =
    [
      in_data "src/toto.ml";
      in_data "src/toto.native";
      in_data "src/stuff/A.cmi";
      in_data "src/stuff/B.cmi";
      in_data "src/stuff/C.cmi";
      in_data "src/stuff/stuff.cma";
      in_data "src/stuff/stuff.cmxa";
    ]
  in

  let () = 
    (* Create temporary file *)
    List.iter (fun fn -> close_out (open_out fn)) tmpfiles;

    at_exit
      (fun () ->
         (* Remove temporary file *)
         List.iter Sys.remove tmpfiles)
  in

  let fn =
    in_data "test1.oasis"
  in

  let ast = 
    OasisTools.parse_file fn
  in

  let env, flags =
    create fn ast
  in

  let pkg =
    oasis (ast, env, flags)
  in

  (* Configuration *)

  let checks pkg = 
    let depends =
      pkg.build_depends
    in
      List.map
        (function
           | pkg, Some ver -> 
               BaseCheck.fenv (BaseCheck.package ~version_comparator:ver pkg)
           | pkg, None ->
               BaseCheck.fenv (BaseCheck.package pkg))
        depends
  in

  let () = 
    prerr_endline "*****************\n\
                   * Configuration *\n\
                   *****************";  

    try
      BaseAction.main
        pkg.name
        pkg.version
        (* Command line argument *)
        [
        ]
        (* Checks*)
        (checks pkg)
        (* .in files *)
        [
        ]
        (* Targets *)
        [
        ]
        (* Packs *)
        [
          BasePack.default
        ]
    with e ->
      prerr_endline (Printexc.to_string e)
  in

  (* Installation *)

  let srcdir = 
    env.srcdir
  in

  let builddir =
    Filename.concat srcdir "_build"
  in

  let bindir =
    "/usr/bin/"
  in

  let rootdirs =
    [srcdir; builddir]
  in

  let ( * ) lst1 lst2 = 
    List.flatten 
      (List.map 
         (fun a -> 
            List.map 
              (fun b -> a,b) 
              lst2) 
         lst1)
  in

  let make_filename =
    function
      | [] -> "" 
      | hd :: tl  -> List.fold_left Filename.concat hd tl
  in

  let make_module nm = 
    [String.capitalize nm; String.uncapitalize nm]
  in

  let find_file f lst = 
    List.find 
      Sys.file_exists
      (List.map (fun e -> make_filename (f e)) lst)
  in

  let find_build_file dir fn =
    find_file
      (fun rootdir -> [rootdir; dir; fn])
      rootdirs
  in

  let install_lib (name, lib) = 
    if lib.lib_buildable then
      (
        let find_build_file =
          find_build_file lib.lib_path
        in

        let module_to_cmi modul =
          find_file 
             (fun (rootdir, fn) -> [rootdir; lib.lib_path; (fn^".cmi")])
             (rootdirs * (make_module modul))
        in

        let module_to_header modul =
          assert(modul <> "");
          find_file 
             (fun ((rootdir, fn), ext) -> [rootdir; lib.lib_path; fn^ext])
             (rootdirs * (make_module modul) * [".mli"; ".ml"])
        in
          
        let cmdline =
          List.flatten
            (
              [
                "ocamlfind"; "install"; name; 
                find_build_file "META";
                find_build_file (name^".cma");
              ]
              :: 
              (
                try 
                  [find_build_file (name^".cmxa")]
                with Not_found ->
                  []
              )
              ::
              (
                List.rev_map
                  (fun modul -> [module_to_cmi modul; module_to_header modul])
                  lib.lib_modules
              )
            )
        in
          prerr_endline (String.concat " " cmdline)
      )
  in

  let install_exec (name, exec) =
    if exec.exec_buildable then
      (
        let exec_file =
          find_file
            (fun ((rootdir, name), ext) -> [rootdir; name^ext])
            (
              rootdirs *
              [name; Filename.chop_extension exec.exec_main_is] *
              [""; ".native"; ".byte"; ".p.native"; ".d.byte"]
            )
        in
        let cmdline =
          [
            "cp"; 
            exec_file; 
            Filename.concat 
              bindir
              name
          ]
        in
          prerr_endline (String.concat " " cmdline)
      )
  in

    prerr_endline "****************\n\
                   * Installation *\n\
                   ****************";  
    List.iter 
      install_lib
      pkg.libraries;

    List.iter
      install_exec
      pkg.executables;

;;

