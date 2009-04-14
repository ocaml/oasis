
open OASISTypes;;
open Format;;
open BaseGenerate;;
open BaseUtils;;

(* Configuration *)
let configure data =

  let pp_compose_lst fmt pp_elem lst =
      fprintf fmt 
        "@[<hv>List.fold_left@, \
          (fun f1 f2 x -> f1 (f2 x))@, \
          (fun x -> x)@, \
          @[[@[@,%a@]@,]@]@]"
        (pp_list pp_elem ";@ ") lst
  in

  let pp_print_checks fmt pkg = 
    pp_compose_lst
      fmt
      (fun fmt e ->
         match e with
           | pkg, Some ver -> 
               fprintf fmt
                 "@[<hv>BaseCheck.fenv@, \
                    (@[BaseCheck.package@, \
                      ~version_comparator:%S@, %S@])@]"
                 pkg
                 ver
           | pkg, None ->
               fprintf fmt
                 "@[<hv>BaseCheck.fenv@, \
                    (@[BaseCheck.package@, %S@])@]"
                 pkg)
      pkg.build_depends
  in

  let pp_print_args fmt flags =
    pp_compose_lst
      fmt
      (fun fmt (nm, flg) ->
         fprintf fmt 
           "@[<hv>BaseArgExt.enable@, %S@, %S@, %B@]"
           nm 
           (match flg.flag_description with
              | Some hlp -> hlp
              | None -> "")
           flg.flag_default)
      flags
  in

  let pp_packs fmt data = 
    pp_record_open fmt ();
    pp_record_field fmt "BasePack.args" pp_print_args data.pre_pkg.flags;
    pp_record_sep fmt ();
    pp_record_field fmt "BasePack.checks" pp_print_checks data.pre_pkg;
    pp_record_sep fmt ();
    pp_record_field fmt "BasePack.in_files" pp_print_string "[]";
    pp_record_close fmt ()
  in
  
  let pp_gen fmt () = 
    fprintf fmt
      "@[<hv>BaseConfigure.configure@, \
         (Filename.dirname Sys.argv.(0))@, \
         %S@, \
         %S@, \
         @[(@[<hv>@,%a@ ::@ []@]@,)@]@]"
      data.pre_pkg.name
      data.pre_pkg.version
      pp_packs data
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

