
open OASISTypes;;
open Format;;
open BaseGenerate;;
open BaseUtils;;

(* Configuration *)
let configure pkg =

  let pp_print_checks fmt pkg = 
    fprintf fmt "[@[<hv2>@,%a@]@,]"
      (pp_list
         (fun fmt e -> e fmt)
         ";@ ")
      (List.flatten 
         [
           List.map 
             (fun e fmt ->
                match e with
                  | pkg, Some ver -> 
                      fprintf fmt
                        "@[<hv2>BaseCheck.package@ ~version_comparator:%S@ %S@]"
                        pkg
                        ver
                  | pkg, None ->
                      fprintf fmt
                        "@[<hv2>BaseCheck.package@, %S@]"
                        pkg)
             pkg.build_depends;
           List.map
             (fun e fmt ->
                fprintf fmt "BaseCheck.prog %S" e)
             pkg.build_tools;
         ])
  in

  let pp_print_args fmt flags =
    fprintf fmt "[@[<hv2>@,%a@]@,]"
      (pp_list
         (fun fmt (nm, flg) ->
            fprintf fmt 
              "@[<hv>BaseArgExt.enable@, %S@, %S@, %B@]"
              nm 
              (match flg.flag_description with
                 | Some hlp -> hlp
                 | None -> "")
              true
         (* TODO: reactivate *)
         (*flg.flag_default*))
         ";@ ")
      flags
  in

  let pp_print_files_ab fmt =
    fprintf fmt "[@[<hv2>@,%a@]@,]"
      (pp_list pp_print_ostring ";@ ")
  in     

  let pp_gen fmt () = 
    fprintf fmt
      "@[<hv2>BaseConfigure.configure@ %S@ %S@ %a@ %a@ %a@]"
      pkg.name
      pkg.version
      pp_print_args pkg.flags
      pp_print_checks pkg
      pp_print_files_ab pkg.files_ab
  in

    {
      moduls           = [BaseData.basesys_ml];
      pp_setup_fun     = pp_gen;
      pp_clean_fun     = None;
      pp_distclean_fun = None;
      other_action     = (fun _ -> ());
    }
;;

configure_generator_register
  "autobuild"
  configure
;;

(* Installation *)
let install =
  (*
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

    List.iter 
      install_lib
      pkg.libraries;

    List.iter
      install_exec
      pkg.executables;
   *)

  (* TODO: really install *)
  BaseGenNone.no_generate Install
;;

generator_register
  Install
  "autobuild"
  install
;;

