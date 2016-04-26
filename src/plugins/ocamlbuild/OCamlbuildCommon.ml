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


(** Functions common to OCamlbuild build and doc plugin
*)


open OASISGettext
open BaseEnv
open BaseStandardVar
open OASISTypes


    TYPE_CONV_PATH "OCamlbuildCommon"

type extra_args = string list with odn


let ocamlbuild_clean_ev = "ocamlbuild-clean"


let ocamlbuildflags =
  var_define
    ~short_desc:(fun () -> "OCamlbuild additional flags")
    "ocamlbuildflags"
    (fun () -> "")


(** Fix special arguments depending on environment *)
let fix_args args extra_argv =
  List.flatten
    [
      if (os_type ()) = "Win32" then
        [
          "-classic-display";
          "-no-log";
          "-no-links";
          "-install-lib-dir";
          (Filename.concat (standard_library ()) "ocamlbuild")
        ]
      else
        [];

      if not (bool_of_string (is_native ())) || (os_type ()) = "Win32" then
        [
          "-byte-plugin"
        ]
      else
        [];
      args;

      if bool_of_string (debug ()) then
        ["-tag"; "debug"]
      else
        [];

      if bool_of_string (tests ()) then
        ["-tag"; "tests"]
      else
        [];

      if bool_of_string (profile ()) then
        ["-tag"; "profile"]
      else
        [];

      OASISString.nsplit (ocamlbuildflags ()) ' ';

      Array.to_list extra_argv;
    ]


(** Run 'ocamlbuild -clean' if not already done *)
let run_clean extra_argv =
  let extra_cli =
    String.concat " " (Array.to_list extra_argv)
  in
  (* Run if never called with these args *)
  if not (BaseLog.exists ocamlbuild_clean_ev extra_cli) then
    begin
      OASISExec.run ~ctxt:!BaseContext.default
        (ocamlbuild ()) (fix_args ["-clean"] extra_argv);
      BaseLog.register ocamlbuild_clean_ev extra_cli;
      at_exit
        (fun () ->
           try
             BaseLog.unregister ocamlbuild_clean_ev extra_cli
           with _ ->
             ())
    end


(** Run ocamlbuild, unregister all clean events *)
let run_ocamlbuild args extra_argv =
  (* TODO: enforce that target in args must be UNIX encoded i.e. toto/index.html
  *)
  OASISExec.run ~ctxt:!BaseContext.default
    (ocamlbuild ()) (fix_args args extra_argv);
  (* Remove any clean event, we must run it again *)
  List.iter
    (fun (e, d) -> BaseLog.unregister e d)
    (BaseLog.filter [ocamlbuild_clean_ev])


(** Determine real build directory *)
let build_dir extra_argv =
  let rec search_args dir =
    function
      | "-build-dir" :: dir :: tl ->
        search_args dir tl
      | _ :: tl ->
        search_args dir tl
      | [] ->
        dir
  in
  search_args "_build" (fix_args [] extra_argv)


(* END EXPORT *)

open OASISValues

let fix_build_tools tool pkg =
  let fix_build_tools' sct bs =
    if not (List.mem tool bs.bs_build_tools) then
      {bs with bs_build_tools = tool :: bs.bs_build_tools}
    else
      bs
  in

  let sections =
    List.fold_left
      (fun acc sct ->
         let sct =
           match sct with
             | Executable (cs, bs, exec) ->
               let bs = fix_build_tools' sct bs in
               Executable (cs, bs, exec)

             | Library (cs, bs, lib) ->
               let bs = fix_build_tools' sct bs in
               Library (cs, bs, lib)

             | Object (cs, bs, obj) ->
               let bs = fix_build_tools' sct bs in
               Object (cs, bs, obj)

             | Flag _ | SrcRepo _ | Test _ | Doc _ as sct ->
               sct
         in
         sct :: acc)
      []
      pkg.sections
  in
  {pkg with sections = List.rev sections}


module Tag =
struct
  (** [filename_concat fn1 fn2] Concat filename, using semantic of _tags
      [fn1] must be a real filename whereas fn2 can contains wildcards.
  *)
  let filename_concat fn1 fn2 =
    OASISUnixPath.concat (OASISUnixPath.reduce fn1) fn2
end

(** Check OCaml version constraint defined in _oasis. *)
let check_ocaml_version version pkg =
  match pkg.ocaml_version with
    | Some ocaml_version ->
      let min_ocaml_version = OASISVersion.version_of_string version in
      OASISVersion.comparator_ge min_ocaml_version ocaml_version
    | None ->
      false


let ocamlbuild_more_args =
  OASISFeatures.create "ocamlbuild_more_args"
    OASISFeatures.alpha
    (fun () ->
       s_ "Allow to pass arguments to ocamlbuild.")


let ocamlbuild_supports_ocamlfind = check_ocaml_version "3.12.1"
let ocamlbuild_supports_plugin_tags = check_ocaml_version "4.01"


type ocamlbuild_common =
  {
    plugin_tags: string option;
    extra_args: string list;
  } with odn


let ocamlbuild_common_generator pivot_data schm id =
  let new_field nm = OASISSchema.new_field schm id nm in
  let plugin_tags =
    new_field
      "PluginTags"
      ~default:None
      ~feature:ocamlbuild_more_args
      (opt string_not_empty)
      (fun () -> s_ "Gives the plugin tags to ocambuild through \
                     '-plugin-tags' (OCaml >= 4.01 only)")
      pivot_data (fun _ t -> t.plugin_tags)
  in
  let extra_args =
    new_field
      "ExtraArgs"
      ~default:[]
      ~feature:ocamlbuild_more_args
      command_line_options
      (fun () -> s_ "Gives extra arguments to ocamlbuild")
      pivot_data (fun _ t -> t.extra_args)
  in
  fun data ->
    {
      extra_args = extra_args data;
      plugin_tags = plugin_tags data;
    }


let extra_args_ocamlbuild_common ~ctxt pkg t =
  let extra_args =
    if t.plugin_tags <> None &&
       not (ocamlbuild_supports_plugin_tags pkg) then begin
      OASISMessage.error
        ~ctxt:ctxt
        (f_ "'XOCamlbuildPluginTags' in only available for OCaml >= 4.01. \
             Please restrict your requirements with 'OCamlVersion: >= 4.01'");
      t.extra_args
    end else begin
      match t.plugin_tags with
        | Some tags -> "-plugin-tags" :: ("'" ^ tags ^ "'") :: t.extra_args
        | None -> t.extra_args
    end
  in
  let extra_args =
    if ocamlbuild_supports_ocamlfind pkg then
      "-use-ocamlfind" :: extra_args
    else
      extra_args
  in
  extra_args
