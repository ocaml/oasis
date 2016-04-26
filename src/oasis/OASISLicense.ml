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


(** License for _oasis fields
    @author Sylvain Le Gall
*)


TYPE_CONV_PATH "OASISLicense"


type license = string with odn


type license_exception = string with odn


type license_version =
  | Version of OASISVersion.t
  | VersionOrLater of OASISVersion.t
  | NoVersion
with odn


type license_dep_5_unit =
  {
    license:   license;
    excption:  license_exception option;
    version:   license_version;
  }
with odn


type license_dep_5 =
  | DEP5Unit of license_dep_5_unit
  | DEP5Or of license_dep_5 list
  | DEP5And of license_dep_5 list
with odn

type t =
  | DEP5License of license_dep_5
  | OtherLicense of string (* URL *)
with odn


(* END EXPORT *)


open OASISValues
open OASISUtils
open OASISGettext


type license_data =
  {
    long_name: string;
    versions: OASISVersion.t list;
    note: string option;
    deprecated: string option;
  }


let all_full_licenses = HashStringCsl.create 64
let deprecated_full_licenses = ref SetStringCsl.empty
let all_licenses = ref []


let string_of_license s = s


let string_of_license_exception s = s


let string_of_license_dep_5 license_dep_5 =
  let ver =
    match license_dep_5.version with
      | Version v ->
        "-"^(OASISVersion.string_of_version v)
      | VersionOrLater v ->
        "-"^(OASISVersion.string_of_version v)^"+"
      | NoVersion ->
        ""
  in
  let exceptions =
    match license_dep_5.excption with
      | None ->
        ""
      | Some str ->
        " with "^str^" exception"
  in
  license_dep_5.license^ver^exceptions


let long_name_of_license_dep_5 license_dep_5 =
  try
    let long_name =
      (List.assoc license_dep_5.license !all_licenses).long_name
    in
    let ver =
      match license_dep_5.version with
        | Version v ->
          " version "^(OASISVersion.string_of_version v)
        | VersionOrLater v ->
          " version "^(OASISVersion.string_of_version v)^" or later"
        | NoVersion ->
          ""
    in
    let exceptions =
      match license_dep_5.excption with
        | None ->
          ""
        | Some str ->
          " with "^str^" exception"
    in
    long_name^ver^exceptions
  with Not_found ->
    string_of_license_dep_5 license_dep_5


let mk_license nm ?(versions=[]) ?deprecated ?note long_name =
  let rec expand_version =
    function
      | hd :: tl ->
        begin
          try
            let hd' =
              OASISString.strip_ends_with ~what:".0" hd
            in
            hd :: (expand_version (hd' :: tl))
          with Not_found ->
            hd :: (expand_version tl)
        end
      | [] ->
        []
  in
  let t =
    {
      long_name = long_name;
      versions =
        List.map
          OASISVersion.version_of_string
          (expand_version versions);
      note = note;
      deprecated = deprecated;
    }
  in
  let license_dep_5 =
    {
      license = nm;
      excption = None;
      version = NoVersion;
    }
  in
  let add_full_license (full_nm, license_dep_5) =
    if HashStringCsl.mem all_full_licenses full_nm then
      failwithf (f_ "Duplicate license '%s'") full_nm;
    HashStringCsl.add all_full_licenses full_nm license_dep_5
  in
  (* Add all expansion of license + version to the hashtable for easy
   * retrieval.
  *)
  add_full_license (nm, license_dep_5);
  List.iter
    (fun ver ->
       let make_nm license_dep_5 =
         (string_of_license_dep_5 license_dep_5), license_dep_5
       in
       add_full_license
         (make_nm {license_dep_5 with version = Version ver});
       add_full_license
         (make_nm {license_dep_5 with version = VersionOrLater ver}))
    t.versions;
  begin
    match deprecated with
      | Some str ->
        add_full_license (str, license_dep_5);
        deprecated_full_licenses :=
          SetStringCsl.add str !deprecated_full_licenses
      | None -> ()
  end;
  (* Append the license to all other licenses. *)
  all_licenses := (nm, t) :: !all_licenses;
  nm


let license_data () =
  let lst =
    List.map
      (fun (license, data) ->
         (license,
          {data with
             (* Really translate strings *)
             long_name = s_ data.long_name;
             note =
               match data.note with
                 | Some str -> Some (s_ str)
                 | None -> None}))
      !all_licenses
  in
  List.sort (fun (nm1, _) (nm2, _) -> compare_csl nm1 nm2) lst


let proprietary =
  mk_license
    "PROP"
    (ns_ "Proprietary license, all rights reserved")


let apache =
  mk_license
    "Apache"
    ~versions:["1.0"; "1.1"; "2.0"]
    (ns_ "Apache license")


let artistic =
  mk_license
    "Artistic"
    ~versions:["1.0"; "2.0"]
    (ns_ "Artistic license")


let bsd2 =
  mk_license
    "BSD-2-clause"
    (ns_ "Berkeley software distribution license (2 clauses)")


let bsd3 =
  mk_license
    "BSD-3-clause"
    ~deprecated:"BSD3"
    (ns_ "Berkeley software distribution license (3 clauses)")


let bsd4 =
  mk_license
    "BSD-4-clause"
    ~deprecated:"BSD4"
    (ns_ "Berkeley software distribution license (4 clauses)")


let cecill =
  mk_license
    "CeCILL"
    ~versions:["1"; "2"]
    ~note:(ns_ "GPL like.")
    (ns_ "CEA-CNRS-INRIA Logiciel Libre")


let cecillb =
  mk_license
    "CeCILL-B"
    (ns_ "CEA-CNRS-INRIA Logiciel Libre, BSD-like")


let cecillc =
  mk_license
    "CeCILL-C"
    (ns_ "CEA-CNRS-INRIA Logiciel Libre, LGPL-like")


let freebsd =
  mk_license
    "FreeBSD"
    "FreeBSD Project license"


let isc =
  mk_license
    "ISC"
    ~note:(ns_ "Sometimes also known as the OpenBSD License.")
    (ns_ "Internet Software Consortium's license")


let cc_by =
  mk_license
    "CC-BY"
    (ns_ "Creative Commons Attribution license")


let cc_by_sa =
  mk_license
    "CC-BY-SA"
    (ns_ "Creative Commons Attribution Share Alike license")


let cc_by_nd =
  mk_license
    "CC-BY-ND"
    (ns_ "Creative Commons Attribution No Derivatives")


let cc_by_nc =
  mk_license
    "CC-BY-NC"
    (ns_ "Creative Commons Attribution Non-Commercial")


let cc_by_nc_sa =
  mk_license
    "CC-BY-NC-SA"
    (ns_ "Creative Commons Attribution Non-Commercial Share Alike")


let cc_by_nc_nd =
  mk_license
    "CC-BY-NC-ND"
    (ns_ "Creative Commons Attribution Non-Commercial No Derivatives")


let cc0 =
  mk_license
    "CC0"
    (ns_ "Creative Commons Universal waiver")


let cddl =
  mk_license
    "CDDL"
    (ns_ "Common Development and Distribution License")


let cpl =
  mk_license
    "CPL"
    ~versions:["1.0"]
    (ns_ "IBM Common Public License")


let eiffel =
  mk_license
    "Eiffel"
    ~versions:["2"]
    (ns_ "The Eiffel Forum License")


let expat =
  mk_license
    "Expat"
    (ns_ "The Expat license")


let gpl =
  mk_license
    "GPL"
    ~versions:["1.0"; "2.0"; "3.0"]
    (ns_ "GNU General Public License")


let lgpl =
  mk_license
    "LGPL"
    ~versions:["2.0"; "2.1"; "3.0"]
    ~note:"GNU Library General Public License for versions lower than 2.1"
    (ns_ "GNU Lesser General Public License")


let agpl =
  mk_license
    "AGPL"
    ~versions:["3.0"]
    (ns_ "GNU Affero General Public License")


let gfdl =
  mk_license
    "GFDL"
    ~versions:["1.1"; "1.2"; "1.3"]
    (ns_ "GNU Free Documentation License")


let gfdl_niv =
  mk_license
    "GFDL-NIV"
    ~versions:["1.1"; "1.2"; "1.3"]
    (ns_ "GNU Free Documentation License, with no invariant sections")


let lppl =
  mk_license
    "LPPL"
    ~versions:["1.3c"]
    (ns_ "LaTeX Project Public License")


let mpl =
  mk_license
    "MPL"
    ~versions:["1.0"; "1.1"]
    (ns_ "Mozilla Public License")


let perl =
  mk_license
    "Perl"
    ~note:(ns_ "Equates to GPL-1+ or Artistic-1.")
    (ns_ "Perl license")


let psf =
  mk_license
    "PSF"
    ~versions:["2"]
    (ns_ "Python Software Foundation license")


let qpl =
  mk_license
    "QPL"
    ~versions:["1.0"]
    (ns_ "Q Public License")


let w3c_software =
  mk_license
    "W3C-Software"
    ~versions:["20021231"]
    (ns_ "W3C Software License")


let zlib =
  mk_license
    "ZLIB"
    ~versions:["1.2.2"]
    (ns_ "zlib/libpng license")


let zope =
  mk_license
    "Zope"
    ~versions:["1.0"; "2.0"; "2.1"]
    (ns_ "Zope Public License")


let mit =
  mk_license
    "MIT"
    (ns_ "MIT License")


let wtfpl =
  mk_license
    "WTFPL"
    (ns_ "Do What The F*ck You Want To Public License")


let public_domain =
  mk_license
    "PD"
    ~note:(ns_ "This is not a true license.")
    (ns_ "Public domain")


type license_exception_data =
  {
    explanation: string;
    licenses:    license list;
  }


let all_exceptions =
  HashStringCsl.create 13


let mk_exception nm explanation licenses =
  if HashStringCsl.mem all_exceptions nm then
    failwithf
      (f_ "Duplicate license exception '%s'")
      nm;
  HashStringCsl.add all_exceptions nm
    {
      explanation = explanation;
      licenses    = licenses;
    };
  nm


let license_exception_data () =
  HashStringCsl.fold
    (fun license_exception data acc ->
       (license_exception,
        {data with
           (* Really translate strings *)
           explanation = s_ data.explanation})
       :: acc)
    all_exceptions
    []


let ocaml_linking_exception =
  mk_exception
    "OCaml linking"
    (ns_ "Add an exception to allow static compilation without license \
          propagation. Without this clause, compiling against an OCaml \
          library force license propagation. The LGPL is equal to GPL \
          without this exception.")
    [lgpl]


let parse_dep_5 ~ctxt str =
  let rec solve_token =
    function
      | OASISLicense_types.Or (t1', t2') ->
        DEP5Or [solve_token t1'; solve_token t2']
      | OASISLicense_types.And (t1', t2') ->
        DEP5And [solve_token t1'; solve_token t2']
      | OASISLicense_types.License (lcs, Some exc) ->
        DEP5Unit {(decode_license lcs) with excption = decode_exception exc}
      | OASISLicense_types.License (lcs, None) ->
        DEP5Unit (decode_license lcs)

  and decode_license str =
    try
      let license_dep_5  = HashStringCsl.find all_full_licenses str in
      if SetStringCsl.mem str !deprecated_full_licenses then
        OASISMessage.warning ~ctxt
          (f_ "Deprecated license %s, use %s instead.")
          str (string_of_license_dep_5 license_dep_5);
      license_dep_5
    with Not_found ->
      failwithf (f_ "Unknown license '%s'") str

  and decode_exception str =
    if not (HashStringCsl.mem all_exceptions str) then
      OASISMessage.warning ~ctxt
        (f_ "Unknown license exception '%s'")
        str;
    Some str
  in

  let rec merge =
    function
      | DEP5Or lst ->
        let lst =
          List.fold_left
            (fun acc ->
               function
                 | DEP5Or lst ->
                   List.rev_append lst acc
                 | DEP5And _ | DEP5Unit _ as t ->
                   t :: acc)
            []
            (List.rev_map merge lst)
        in
        DEP5Or lst

      | DEP5And lst ->
        let lst =
          List.fold_left
            (fun acc ->
               function
                 | DEP5And lst ->
                   List.rev_append lst acc
                 | DEP5Or _ | DEP5Unit _ as t ->
                   t :: acc)
            []
            (List.rev_map merge lst)
        in
        DEP5And lst

      | DEP5Unit _ as t ->
        t
  in

  let lexbuf = Lexing.from_string str in
  let t' = OASISLicense_parser.main OASISLicense_lexer.token lexbuf in
  merge (solve_token t')


let parse ~ctxt str =
  try
    OtherLicense
      (OASISValues.url.parse ~ctxt str)
  with Failure _ ->
  try
    DEP5License (parse_dep_5 ~ctxt str)
  with e ->
    begin
      failwithf
        (f_ "Cannot parse license '%s': %s")
        str (Printexc.to_string e)
    end


let rec string_of_dep_5_generic f dep_5 =
  let rec dep_5_str =
    function
      | DEP5Unit t  ->
        f t

      | DEP5Or lst ->
        String.concat " or " (List.map dep_5_str lst)

      | DEP5And [DEP5Or _ as t1; DEP5Unit _ as t2] ->
        (dep_5_str t1)^", and "^(dep_5_str t2)

      | DEP5And lst ->
        String.concat " and "
          (List.map
             (fun t ->
                let str = dep_5_str t in
                match t with
                  | DEP5Or _ ->
                    "("^str^")"
                  | _ ->
                    str)
             lst)
  in
  dep_5_str dep_5


let to_string =
  function
    | DEP5License dep_5 ->
      string_of_dep_5_generic string_of_license_dep_5 dep_5
    | OtherLicense url -> url


let legal_disclaimer nm =
  function
    | DEP5License dep_5 ->
      Printf.sprintf
        "%s is distributed under the terms of the %s."
        nm (string_of_dep_5_generic long_name_of_license_dep_5 dep_5)
    | OtherLicense url ->
      Printf.sprintf
        "%s is distributed using the license describe at this [URL](%s)."
        nm url


let value =
  {
    parse  = parse;
    update = OASISValues.update_fail;
    print  = to_string;
  }


let choices () =
  let compare_license t1 t2 =
    match compare_csl t1.license t2.license with
      | 0 ->
        begin
          let v_cmp =
            match t1.version, t2.version with
              | NoVersion, NoVersion ->
                0
              | NoVersion, _ ->
                -1
              | _, NoVersion ->
                1
              | Version v1, Version v2
              | VersionOrLater v1, Version v2
              | Version v1, VersionOrLater v2
              | VersionOrLater v1, VersionOrLater v2 ->
                OASISVersion.version_compare v1 v2
          in
          match v_cmp with
            | 0 ->
              begin
                compare t1.excption t2.excption
              end
            | n ->
              n
        end
      | n ->
        n
  in

  let exception_find license mp =
    try
      MapString.find license mp
    with Not_found ->
      []
  in

  let exceptions_map =
    HashStringCsl.fold
      (fun excpt data mp ->
         List.fold_left
           (fun mp license ->
              let vl =
                exception_find license mp
              in
              MapString.add license (excpt :: vl) mp)
           mp
           data.licenses)
      all_exceptions
      MapString.empty
  in

  let all =
    List.fold_left
      (fun acc (license, data) ->
         let dflt =
           {license = license; version = NoVersion; excption = None}
         in
         List.fold_left
           (fun acc ver ->
              let dflt =
                {dflt with version = Version ver}
              in
              List.fold_left
                (fun acc excpt ->
                   {dflt with excption = Some excpt} :: acc)
                (* only versions *)
                (dflt :: acc)
                (exception_find license exceptions_map))
           (* only license *)
           (dflt :: acc)
           data.versions)
      []
      !all_licenses
  in

  let all =
    List.sort compare_license all
  in

  let preferred =
    [
      {license = lgpl;
       version = Version (OASISVersion.version_of_string "2.1");
       excption = Some ocaml_linking_exception};
      {license = bsd3;
       version = NoVersion;
       excption = None};
      {license = gpl;
       version =  Version (OASISVersion.version_of_string "3.0");
       excption = None};
      {license = agpl;
       version =  Version (OASISVersion.version_of_string "3.0");
       excption = None};
      {license = qpl;
       version = Version (OASISVersion.version_of_string "1.0");
       excption = None};
      {license = mit;
       version = NoVersion;
       excption = None};
    ]
  in
  let all =
    preferred @ (List.filter (fun l -> not (List.mem l preferred)) all)
  in
  List.map (fun t -> DEP5License (DEP5Unit t)) all
