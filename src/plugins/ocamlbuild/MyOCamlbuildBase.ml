(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

(** Base functions for writing myocamlbuild.ml
    @author Sylvain Le Gall
  *)

TYPE_CONV_PATH "MyOCamlbuildBase"

open Ocamlbuild_plugin
module OC = Ocamlbuild_pack.Ocaml_compiler

type dir = string with odn
type file = string with odn
type name = string with odn
type tag = string with odn

(* END EXPORT *)
let rec odn_of_spec =
  let vrt nm lst = 
    ODN.VRT ("Ocamlbuild_plugin."^nm, lst)
  in
  let vrt_str nm str =
    vrt nm [ODN.STR str]
  in
    function
      | N     -> vrt "N" []
      | S lst -> vrt "S" [ODN.of_list odn_of_spec lst]
      | A s   -> vrt_str "A" s
      | P s   -> vrt_str "P" s
      | Px s  -> vrt_str "Px" s
      | Sh s  -> vrt_str "Sh" s
      | V s   -> vrt_str "V" s
      | Quote spc -> vrt "Quote" [odn_of_spec spc]
      | T _ -> 
          assert false
(* START EXPORT *)

type t =
    {
      lib_ocaml: (name * dir list) list;
      lib_c:     (name * dir * file list) list; 
      flags:     (tag list * (spec OASISExpr.choices)) list;
      (* Replace the 'dir: include' from _tags by a precise interdepends in
       * directory.
       *)
      includes:  (dir * dir list) list; 
    } with odn

let env_filename =
  Pathname.basename 
    BaseEnvLight.default_filename

let dispatch_combine lst =
  fun e ->
    List.iter 
      (fun dispatch -> dispatch e)
      lst 

let tag_libstubs nm =
  "use_lib"^nm^"_stubs"

let nm_libstubs nm =
  nm^"_stubs"

let dispatch t e = 
  let env = 
    BaseEnvLight.load 
      ~filename:env_filename 
      ~allow_empty:true
      ()
  in
    match e with 
      | Before_options ->
          let no_trailing_dot s =
            if String.length s >= 1 && s.[0] = '.' then
              String.sub s 1 ((String.length s) - 1)
            else
              s
          in
            List.iter
              (fun (opt, var) ->
                 try 
                   opt := no_trailing_dot (BaseEnvLight.var_get var env)
                 with Not_found ->
                   Printf.eprintf "W: Cannot get variable %s" var)
              [
                Options.ext_obj, "ext_obj";
                Options.ext_lib, "ext_lib";
                Options.ext_dll, "ext_dll";
              ]

      | Before_rules ->
        (* TODO: move this into its own file and conditionnaly include it, if
         * needed.
         *)
        (* OCaml cmxs rules: cmxs available in ocamlopt but not ocamlbuild.
           Copied from ocaml_specific.ml in ocamlbuild sources. *)
        let has_native_dynlink =
          try
            bool_of_string (BaseEnvLight.var_get "native_dynlink" env)
          with Not_found ->
            false
        in
        if has_native_dynlink && String.sub Sys.ocaml_version 0 4 = "3.11" then
          begin
            let ext_lib = !Options.ext_lib in
            let ext_obj = !Options.ext_obj in
            let ext_dll = !Options.ext_dll in
            let x_o = "%"-.-ext_obj in
            let x_a = "%"-.-ext_lib in
            let x_dll = "%"-.-ext_dll in
            let x_p_o = "%.p"-.-ext_obj in
            let x_p_a = "%.p"-.-ext_lib in
            let x_p_dll = "%.p"-.-ext_dll in

            rule "ocaml: mldylib & p.cmx* & p.o* -> p.cmxs & p.so"
                 ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                 ~prods:["%.p.cmxs"; x_p_dll]
                 ~dep:"%.mldylib"
                 (OC.native_profile_shared_library_link_mldylib
                    "%.mldylib" "%.p.cmxs");

            rule "ocaml: mldylib & cmx* & o* -> cmxs & so"
                 ~tags:["ocaml"; "native"; "shared"; "library"]
                 ~prods:["%.cmxs"; x_dll]
                 ~dep:"%.mldylib"
                 (OC.native_shared_library_link_mldylib
                    "%.mldylib" "%.cmxs");

            rule "ocaml: p.cmx & p.o -> p.cmxs & p.so"
                 ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                 ~prods:["%.p.cmxs"; x_p_dll]
                 ~deps:["%.p.cmx"; x_p_o]
                 (OC.native_shared_library_link ~tags:["profile"]
                                                "%.p.cmx" "%.p.cmxs");

            rule "ocaml: p.cmxa & p.a -> p.cmxs & p.so"
                 ~tags:["ocaml"; "native"; "profile"; "shared"; "library"]
                 ~prods:["%.p.cmxs"; x_p_dll]
                 ~deps:["%.p.cmxa"; x_p_a]
                 (OC.native_shared_library_link ~tags:["profile"; "linkall"]
                                                "%.p.cmxa" "%.p.cmxs");

            rule "ocaml: cmx & o -> cmxs"
                 ~tags:["ocaml"; "native"; "shared"; "library"]
                 ~prods:["%.cmxs"]
                 ~deps:["%.cmx"; x_o]
                 (OC.native_shared_library_link "%.cmx" "%.cmxs");

            rule "ocaml: cmx & o -> cmxs & so"
                 ~tags:["ocaml"; "native"; "shared"; "library"]
                 ~prods:["%.cmxs"; x_dll]
                 ~deps:["%.cmx"; x_o]
                 (OC.native_shared_library_link "%.cmx" "%.cmxs");

            rule "ocaml: cmxa & a -> cmxs & so"
                 ~tags:["ocaml"; "native"; "shared"; "library"]
                 ~prods:["%.cmxs"; x_dll]
                 ~deps:["%.cmxa"; x_a]
                 (OC.native_shared_library_link ~tags:["linkall"]
                                                "%.cmxa" "%.cmxs");
          end

      | After_rules -> 
          (* Declare OCaml libraries *)
          List.iter 
            (function
               | nm, [] ->
                   ocaml_lib nm
               | nm, dir :: tl ->
                   ocaml_lib ~dir:dir (dir^"/"^nm);
                   List.iter 
                     (fun dir -> 
                        List.iter
                          (fun str ->
                             flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                          ["compile"; "infer_interface"; "doc"])
                     tl)
            t.lib_ocaml;

          (* Declare directories dependencies, replace "include" in _tags. *)
          List.iter 
            (fun (dir, include_dirs) ->
               Pathname.define_context dir include_dirs)
            t.includes;

          (* Declare C libraries *)
          List.iter
            (fun (lib, dir, headers) ->
                 (* Handle C part of library *)
                 flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                   (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                      A("-l"^(nm_libstubs lib))]);

                 flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                   (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);
                      
                 flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                   (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);

                 (* When ocaml link something that use the C library, then one
                    need that file to be up to date.
                  *)
                 dep  ["link"; "ocaml"; "program"; tag_libstubs lib]
                   [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                 dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                   [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                 (* TODO: be more specific about what depends on headers *)
                 (* Depends on .h files *)
                 dep ["compile"; "c"] 
                   headers;

                 (* Setup search path for lib *)
                 flag ["link"; "ocaml"; "use_"^lib] 
                   (S[A"-I"; P(dir)]);
            )
            t.lib_c;

            (* Add flags *)
            List.iter
            (fun (tags, cond_specs) ->
               let spec = 
                 BaseEnvLight.var_choose cond_specs env
               in
                 flag tags & spec)
            t.flags
      | _ -> 
          ()

let dispatch_default t =
  dispatch_combine 
    [
      dispatch t;
      MyOCamlbuildFindlib.dispatch;
    ]

(* END EXPORT *)
