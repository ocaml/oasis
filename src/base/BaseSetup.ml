(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Entry points for setup.ml
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISMessage
open OASISTypes
open OASISSection
open OASISGettext
open OASISUtils

type std_args_fun = 
    package -> string array -> unit

type ('a, 'b) section_args_fun = 
    name * (package -> (common_section * 'a) -> string array -> 'b)

type t =
    {
      configure:       std_args_fun;
      build:           std_args_fun;
      doc:             ((doc, unit)  section_args_fun) list;
      test:            ((test, float) section_args_fun) list;
      install:         std_args_fun;
      uninstall:       std_args_fun;
      clean:           std_args_fun list;
      clean_doc:       (doc, unit) section_args_fun list;
      clean_test:      (test, unit) section_args_fun list;
      distclean:       std_args_fun list;
      distclean_doc:   (doc, unit) section_args_fun list;
      distclean_test:  (test, unit) section_args_fun list;
      package:         package;
    } 

(** Associate a plugin function with data from package
  *)
let join_plugin_sections filter_map lst =
  List.rev
    (List.fold_left
       (fun acc sct ->
          try 
            match filter_map sct with 
              | Some e ->
                  e :: acc
              | None ->
                  acc
          with Not_found ->
            failwithf1
              (f_ "Cannot find plugin matching %s")
              (OASISSection.string_of_section sct))
       []
       lst)

(** Configure step *)
let configure t args = 
  (* Run configure *)
  BaseCustom.hook 
    t.package.conf_custom
    (t.configure t.package)
    args;

  (* Reload environment *)
  unload ();
  load ();

  (* Replace data in file *)
  BaseFileAB.replace t.package.files_ab

(** Build step *)
let build t args =
  BaseCustom.hook
    t.package.build_custom
    (t.build t.package)
    args

(** Documentation step *)
let doc t args =
  BaseDoc.doc
    (join_plugin_sections
       (function 
          | Doc (cs, e) -> 
              Some 
                (List.assoc cs.cs_name t.doc,
                 cs,
                 e)
          | _ -> 
              None)
       t.package.sections)
    t.package
    args

(** Test step *)
let test t args = 
  BaseTest.test 
    (join_plugin_sections
       (function 
          | Test (cs, e) -> 
              Some 
                (List.assoc cs.cs_name t.test,
                 cs,
                 e)
          | _ -> 
              None)
       t.package.sections)
    t.package
    args

(** Install step *)
let install t args =
  BaseCustom.hook
    t.package.install_custom
    (t.install t.package)
    args

(** Uninstall step *)
let uninstall t args =
  BaseCustom.hook
    t.package.uninstall_custom
    (t.uninstall t.package)
    args

(** Clean and distclean steps *)
let clean, distclean = 
  let generic_clean t what cstm mains docs tests args = 
    BaseCustom.hook
      ~failsafe:true
      cstm
      (fun () ->
         (* Clean section *)
         List.iter
           (function
              | Test (cs, test) ->
                  let f =
                    List.assoc cs.cs_name tests
                  in
                    f t.package (cs, test) args
              | Library _ 
              | Executable _
              | Flag _ 
              | SrcRepo _
              | Doc _ ->
                  ())
           t.package.sections;
         (* Clean whole package *)
         List.iter
           (fun f -> 
              f t.package args)
           mains)
      ()
  in

  let clean t args =
    generic_clean 
      t 
      "cleaning" 
      t.package.clean_custom
      t.clean 
      t.clean_doc 
      t.clean_test 
      args
  in

  let distclean t args =
    (* Call clean *)
    clean t args;

    (* Remove generated file *)
    List.iter
      (fun fn ->
         if Sys.file_exists fn then
           begin
             info (f_ "Remove '%s'") fn;
             Sys.remove fn
           end)
      (BaseEnv.default_filename 
       :: 
       BaseLog.default_filename
       ::
       (List.rev_map BaseFileAB.to_filename t.package.files_ab));
    
    (* Call distclean code *)
    generic_clean 
      t 
      "distcleaning" 
      t.package.distclean_custom
      t.distclean 
      t.distclean_doc 
      t.distclean_test 
      args
  in

    clean, distclean

let setup t = 
  let catch_exn =
    ref true
  in
    try
      let act_ref =
        ref (fun _ -> 
               failwithf2
                 (f_ "No action defined, run '%s %s -help'")
                 Sys.executable_name
                 Sys.argv.(0))

      in
      let extra_args_ref =
        ref []
      in
      let allow_empty_env_ref = 
        ref false
      in
      let arg_handle ?(allow_empty_env=false) act =
        Arg.Tuple
          [
            Arg.Rest (fun str -> extra_args_ref := str :: !extra_args_ref);

            Arg.Unit 
              (fun () -> 
                 allow_empty_env_ref := allow_empty_env;
                 act_ref := act);
          ]
      in

        Arg.parse 
          (Arg.align
             [
               "-configure",
               arg_handle ~allow_empty_env:true configure,
               s_ "[options*] Configure build process.";

               "-build",
               arg_handle build,
               s_ "[options*] Run build process.";

               "-doc",
               arg_handle doc,
               s_ "[options*] Build documentation.";

               "-test",
               arg_handle test,
               s_ "[options*] Run tests.";

               "-install",
               arg_handle install,
               s_ "[options*] Install libraries, data, executables \
                              and documentations.";

               "-uninstall",
               arg_handle uninstall,
               s_ "[options*] Uninstall libraries, data, executables \
                              and documentations.";

               "-clean",
               arg_handle ~allow_empty_env:true clean,
               s_ "[options*] Clean build environment.";

               "-distclean",
               arg_handle ~allow_empty_env:true distclean,
               s_ "[options*] Clean build and configure environment.";

               "-no-catch-exn",
               Arg.Clear catch_exn,
               s_ " Don't catch exception, useful for debugging.";
             ] 
           @ (OASISMessage.args ()))
          (failwithf1 (f_ "Don't know what to do with '%s'"))
          (s_ "Setup and run build process current package\n");

        (* Build initial environment *)
        load ~allow_empty:!allow_empty_env_ref ();

        (** Initialize flags *)
        List.iter
          (function
             | Flag (cs, {flag_description = hlp; 
                          flag_default = choices}) ->
                 begin
                   let apply ?short_desc () = 
                     var_ignore
                       (var_define
                          ~cli:CLIEnable
                          ?short_desc
                          cs.cs_name
                          (lazy (string_of_bool 
                                   (var_choose choices))))
                   in
                     match hlp with 
                       | Some hlp ->
                           apply ~short_desc:(fun () -> hlp) ()
                       | None ->
                           apply ()
                 end
             | _ -> 
                 ())
          t.package.sections;

        BaseStandardVar.init t.package;

        BaseDynVar.init t.package;

        !act_ref t (Array.of_list (List.rev !extra_args_ref))

    with e when !catch_exn ->
      begin
        try 
          error "%s" (PropList.string_of_exception e)
        with 
          | Failure s ->
              error "%s" s
          | e ->
              error "%s" (Printexc.to_string e)
      end

(* END EXPORT *)

module PLG = OASISPlugin

let odn_of_oasis pkg = 

  let build_gen, pkg = 
    (PLG.Build.find pkg.build_type) pkg
  in

  let test_odn, test_gens, pkg = 
    let test_odns, test_gens, pkg = 
      List.fold_left
        (fun (test_odns, test_gens, pkg) -> 
           function
             | Test (cs, tst) ->
                 begin
                   let gen, pkg, cs, tst = 
                     (PLG.Test.find tst.test_type) pkg (cs, tst)
                   in
                     (ODN.TPL [ODN.STR cs.cs_name; 
                               ODNFunc.odn_of_func gen.PLG.setup] 
                      :: 
                      test_odns),
                     (cs.cs_name, gen) :: test_gens,
                     {pkg with sections = (Test (cs, tst)) :: pkg.sections}
                 end
             | sct ->
                 test_odns,
                 test_gens,
                 {pkg with sections = sct :: pkg.sections})
        ([], [], {pkg with sections = []})
        pkg.sections
    in
      ODN.LST (List.rev test_odns),
      List.rev test_gens,
      {pkg with sections = List.rev pkg.sections}
  in

  let doc_odn, doc_gens, pkg = 
    let doc_odns, doc_gens, pkg =
      List.fold_left
        (fun (doc_odns, doc_gens, pkg) -> 
           function
             | Doc (cs, doc) ->
                 begin
                   let gen, pkg, cs, doc = 
                     (PLG.Doc.find doc.doc_type) pkg (cs, doc)
                   in
                     (ODN.TPL [ODN.STR cs.cs_name; 
                               ODNFunc.odn_of_func gen.PLG.setup] 
                      :: 
                      doc_odns),
                     (cs.cs_name, gen) :: doc_gens,
                     {pkg with sections = (Doc (cs, doc)) :: pkg.sections}
                 end
             | sct ->
                 doc_odns,
                 doc_gens,
                 {pkg with sections = sct :: pkg.sections})
        ([], [], {pkg with sections = []})
        pkg.sections
    in
      ODN.LST (List.rev doc_odns),
      List.rev doc_gens,
      {pkg with sections = List.rev pkg.sections}
  in

  let install_gen, uninstall_gen, pkg =
    let inst, uninst = 
      PLG.Install.find pkg.install_type
    in
    let install_gen, pkg = 
      inst pkg
    in
    let uninstall_gen, pkg =
      uninst pkg
    in
      install_gen, uninstall_gen, pkg
  in

  let configure_gen, pkg =
    (PLG.Configure.find pkg.conf_type) pkg
  in

  let std_gens = 
    [
      configure_gen;
      build_gen;
      install_gen;
      uninstall_gen;
    ]
  in

  let moduls = 
    let moduls_of_gen gen =
      gen.PLG.moduls
    in
    let moduls_of_gens lst =
      List.rev_map 
        moduls_of_gen 
        (List.rev_map snd lst)
    in
      List.flatten
        (List.flatten
           [
             List.map moduls_of_gen std_gens;
             moduls_of_gens doc_gens;
             moduls_of_gens test_gens;
           ])
  in

  let other_actions = 
    let of_gen gen =
      gen.PLG.other_action
    in
    let of_gens lst = 
      List.rev_map of_gen (List.rev_map snd lst)
    in
      List.flatten
        [
          List.map of_gen std_gens;
          of_gens doc_gens;
          of_gens test_gens;
        ]
  in

  let clean_funcs,     clean_doc_funcs,     clean_test_funcs,
      distclean_funcs, distclean_doc_funcs, distclean_test_funcs =
    let acc_non_opt f lst = 
      let acc_non_opt_aux acc e = 
        match f e with
          | Some v -> v :: acc
          | None -> acc
      in
        List.rev (List.fold_left acc_non_opt_aux [] lst)
    in

    let acc_non_opt_assoc f =
      let assoc_f (nm, e) =
        match f e with
          | Some v -> Some (nm, v)
          | None -> None
      in
        acc_non_opt assoc_f 
    in

    let clean_of_gen gen = gen.PLG.clean
    in
    let distclean_of_gen gen = gen.PLG.distclean
    in

      acc_non_opt clean_of_gen std_gens,
      acc_non_opt_assoc clean_of_gen doc_gens,
      acc_non_opt_assoc clean_of_gen test_gens,
      acc_non_opt distclean_of_gen std_gens,
      acc_non_opt_assoc distclean_of_gen doc_gens,
      acc_non_opt_assoc distclean_of_gen test_gens
  in

  let () = 
    (* Put some type constraints between plugin and
       BaseSetup.t
     *)
    if false then
      begin
        let setup_func_calls lst =
          List.map (fun (nm, gen) -> nm, ODNFunc.func_call gen.PLG.setup) lst
        in
        let func_calls lst = 
          List.map (fun (nm, func) -> nm, ODNFunc.func_call func) lst
        in
        let _t: t = 
          {
            configure       = ODNFunc.func_call configure_gen.PLG.setup;
            build           = ODNFunc.func_call build_gen.PLG.setup;
            doc             = setup_func_calls doc_gens;
            test            = setup_func_calls test_gens;
            install         = ODNFunc.func_call install_gen.PLG.setup;
            uninstall       = ODNFunc.func_call uninstall_gen.PLG.setup;
            clean           = List.map ODNFunc.func_call clean_funcs;
            clean_test      = func_calls clean_test_funcs;
            clean_doc       = func_calls clean_doc_funcs;
            distclean       = List.map ODNFunc.func_call distclean_funcs;
            distclean_test  = func_calls distclean_test_funcs;
            distclean_doc   = func_calls distclean_doc_funcs;
            package         = pkg;
          } 
        in
          ()
      end
  in

  let setup_t_odn =
    let odn_of_funcs lst =
      ODN.LST (List.map ODNFunc.odn_of_func lst)
    in
    let odn_of_assocs lst =
      ODN.LST 
        (List.map 
           (fun (nm, func) -> 
              ODN.TPL[ODN.STR nm; ODNFunc.odn_of_func func])
           lst)
    in
      ODN.REC
        ("BaseSetup",
         [
           "configure",      ODNFunc.odn_of_func configure_gen.PLG.setup;
           "build",          ODNFunc.odn_of_func build_gen.PLG.setup;
           "test",           test_odn;
           "doc",            doc_odn;
           "install",        ODNFunc.odn_of_func  install_gen.PLG.setup;
           "uninstall",      ODNFunc.odn_of_func  uninstall_gen.PLG.setup;
           "clean",          odn_of_funcs clean_funcs;
           "clean_test",     odn_of_assocs clean_test_funcs;
           "clean_doc",      odn_of_assocs clean_doc_funcs;
           "distclean",      odn_of_funcs distclean_funcs;
           "distclean_test", odn_of_assocs distclean_test_funcs;
           "distclean_doc",  odn_of_assocs distclean_doc_funcs;
           "package",        OASISTypes.odn_of_package pkg;
          ])
  in

    pkg, setup_t_odn, other_actions, moduls
