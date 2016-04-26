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


(* The content of this file started with ocsigen_loader.ml
 * from the ocsigen project (http://www.ocsigen.org).
 *
 * It was:
 * Copyright (C) 2008 Stéphane Glondu
*)


exception Dynlink_error of string * exn
exception Findlib_error of string * exn
exception Plugin_not_found of string


(**/**)
(** TODO: Gettext related functions, to be replaced by real ones. *)
let s_ s = s
let f_ fmt = ""^^fmt
(**/**)


(* Error formatting *)


open Printf


let () =
  Printexc.register_printer
    (function
      | Dynlink_error (s, Dynlink.Error e) ->
        Some
          (sprintf (f_ "Dynlink error while loading '%s': %s")
             s (Dynlink.error_message e))

      | Findlib_error (s, Fl_package_base.No_such_package (s', msg)) ->
        let pkg =
          if s = s' then
            "'"^s^"'"
          else
            sprintf (f_ "'%s' [while trying to load '%s']") s' s
        in
        let additional =
          if msg = "" then "" else sprintf " (%s)" msg
        in
        Some
          (sprintf (f_ "Findlib package %s not found%s")
             pkg additional)

      | Findlib_error (s, e) ->
        Some (sprintf "Findlib error while handling '%s': %s"
            s (Printexc.to_string e))
      | Plugin_not_found nm ->
        Some (sprintf (f_ "Plugin '%s' not found") nm)
      | _ ->
        None);


module StringSet = Set.Make(String)


(* Loading files *)


module SetString = Set.Make(String)


let findlib_packages_loaded = ref SetString.empty


let add_findlib_package e =
  findlib_packages_loaded := SetString.add e !findlib_packages_loaded

(* Fake object, to keep in the generated program a reference to CamlinternalOO.
*)
class foo = object end


let init findlib_packages_loaded =
  (* TODO: only_once *)
  List.iter add_findlib_package findlib_packages_loaded;
  Findlib.init ()


type 'a t =
  {
    system: string;
    msg: ([>`Debug | `Warning | `Error] as 'a) -> string -> unit;
  }


type entry =
  {
    findlib_name: string;
    name: string;
    synopsis: string option;
    version: string option;
    deprecated: bool;
  }


(* Using Findlib to locate files *)
let findfiles t package =
  let rev_split_blank str =
    let buf = Buffer.create 13 in
    let lst = ref [] in
    String.iter
      (function
        | ' ' ->
          if Buffer.length buf > 0 then
            begin
              lst := Buffer.contents buf :: !lst;
              Buffer.clear buf
            end
        | c ->
          Buffer.add_char buf c)
      str;
    begin
      match Buffer.contents buf with
        | "" -> ()
        | str -> lst := str :: !lst
    end;
    !lst
  in

  try
    let preds =
      [if Dynlink.is_native then "native" else "byte"]
    in
    let deps =
      List.filter
        (fun a -> not (SetString.mem a !findlib_packages_loaded))
        (Findlib.package_deep_ancestors preds [package])
    in
    t.msg
      `Debug
      (sprintf
         (f_ "Dependencies of %s: %s")
         package (String.concat ", " deps));
    let rec aux =
      function
        | [] -> []
        | a :: tl ->
          let mods =
            try
              let raw =
                Findlib.package_property ("plugin" :: preds) a "archive"
              in
              List.rev (rev_split_blank raw)
            with Not_found ->
              begin
                try
                  let raw = Findlib.package_property preds a "archive" in
                  List.rev_map
                    (fun fn ->
                       (* Replacing .cmx/.cmxa by .cmxs *)
                       if Dynlink.is_native &&
                          (Filename.check_suffix fn "cmx" ||
                           Filename.check_suffix fn "cmxa") then
                         (Filename.chop_extension fn) ^ ".cmxs"
                       else
                         fn)
                    (rev_split_blank raw)
                with Not_found ->
                  begin
                    t.msg `Error
                      (sprintf
                         (f_ "Cannot find 'archive' attribute for findlib \
                              package %s")
                         a);
                    []
                  end
              end
          in
          let base = Findlib.package_directory a in
          add_findlib_package a;
          (List.map (Findlib.resolve_path ~base) mods) @ (aux tl)
    in

    let res = aux deps in
    t.msg `Debug (sprintf "Object files needed: %s"
        (String.concat ", " res));
    res

  with e ->
    raise (Findlib_error (package, e))


module SetEntry =
  Set.Make
    (struct
      type t = entry
      let compare e1 e2 =
        String.compare e1.name e2.name
    end)


let list t =
  let lst = Fl_package_base.list_packages () in
  let set =
    List.fold_left
      (fun acc pkg_str ->
         try
           let pkg = Fl_package_base.query pkg_str in
           let package_defs = pkg.Fl_package_base.package_defs in
           let plugin_system =
             Fl_metascanner.lookup "plugin_system" [] package_defs
           in
           let default_lookup var =
             try
               Some (Fl_metascanner.lookup var [] package_defs)
             with Not_found ->
               None
           in
           let default_lookup_val var dflt =
             match default_lookup var with
               | Some str -> str
               | None -> dflt
           in
           if plugin_system = t.system then begin
             let deprecated =
               let str =
                 default_lookup_val "plugin_deprecated" "false"
               in
               try
                 bool_of_string str
               with Invalid_argument _ ->
                 t.msg `Warning
                   (sprintf "Field plugin_deprecated of plugin '%s' \
                             should be true or false, got %s."
                      pkg_str str);
                 false
             in
             let entry =
               {
                 findlib_name = pkg_str;
                 name = default_lookup_val "plugin_name" pkg_str;
                 synopsis = default_lookup "plugin_synopsis";
                 version = default_lookup "version";
                 deprecated = deprecated;
               }
             in
             if SetEntry.mem entry acc then
               t.msg `Warning
                 (sprintf
                    (f_ "Plugin '%s' already defined \
                         (findlib name: %s; directory: '%s').")
                    entry.name
                    pkg_str
                    pkg.Fl_package_base.package_dir);
             SetEntry.add entry acc
           end else begin
             acc
           end
         with e ->
           acc)
      SetEntry.empty
      lst
  in
  SetEntry.elements set


let load t nm =
  (* TODO: critical section. *)
  let entry =
    try
      List.find (fun e -> e.name = nm) (list t)
    with Not_found ->
      raise (Plugin_not_found nm)
  in
  let lst =
    findfiles t entry.findlib_name
  in
  try
    List.iter Dynlink.loadfile lst
  with e ->
    raise (Dynlink_error (nm, e))


