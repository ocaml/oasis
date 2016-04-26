(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
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
    lib_ocaml: (name * dir list * string list) list;
    lib_c:     (name * dir * file list) list;
    flags:     (tag list * (spec OASISExpr.choices)) list;
    (* Replace the 'dir: include' from _tags by a precise interdepends in
     * directory.
    *)
    includes:  (dir * dir list) list;
  } with odn


let env_filename =
  lazy (Pathname.basename (Lazy.force BaseEnvLight.default_filename))


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
      ~filename:(Lazy.force env_filename)
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
             Printf.eprintf "W: Cannot get variable %s\n" var)
        [
          Options.ext_obj, "ext_obj";
          Options.ext_lib, "ext_lib";
          Options.ext_dll, "ext_dll";
        ]

    | After_rules ->
      (* Declare OCaml libraries *)
      List.iter
        (function
          | nm, [], intf_modules ->
            ocaml_lib nm;
            let cmis =
              List.map (fun m -> (String.uncapitalize m) ^ ".cmi")
                intf_modules in
            dep ["ocaml"; "link"; "library"; "file:"^nm^".cma"] cmis
          | nm, dir :: tl, intf_modules ->
            ocaml_lib ~dir:dir (dir^"/"^nm);
            List.iter
              (fun dir ->
                 List.iter
                   (fun str ->
                      flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                   ["compile"; "infer_interface"; "doc"])
              tl;
            let cmis =
              List.map (fun m -> dir^"/"^(String.uncapitalize m)^".cmi")
                intf_modules in
            dep ["ocaml"; "link"; "library"; "file:"^dir^"/"^nm^".cma"]
              cmis)
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
              This holds both for programs and for libraries.
           *)
           dep ["link"; "ocaml"; tag_libstubs lib]
             [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

           dep  ["compile"; "ocaml"; tag_libstubs lib]
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
           let spec = BaseEnvLight.var_choose cond_specs env in
           let rec eval_specs =
             function
               | S lst -> S (List.map eval_specs lst)
               | A str -> A (BaseEnvLight.var_expand str env)
               | spec -> spec
           in
           flag tags & (eval_specs spec))
        t.flags
    | _ ->
      ()


let dispatch_default conf t =
  dispatch_combine
    [
      dispatch t;
      MyOCamlbuildFindlib.dispatch conf;
    ]


(* END EXPORT *)
