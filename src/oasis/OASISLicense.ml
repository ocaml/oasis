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

type license_dep_5 = 
    {
      license:    license;
      exceptions: license_exception list;
      version:    license_version;
    } with odn

type t =
  | DEP5License of license_dep_5
  | OtherLicense of string (* URL *)
  with odn

(* END EXPORT *)

open OASISValues
open OASISUtils
open OASISGettext

let string_of_license s = s

let string_of_license_exception s = s

type license_data = 
    {
      long_name: string;
      versions:  OASISVersion.t list;
      note:      string option;
    }

let all_licenses = 
  HashStringCsl.create 13

let mk_license nm ?(versions=[]) ?note long_name = 
  if HashStringCsl.mem all_licenses nm then
    failwithf1
      (f_ "Duplicate license '%s'")
      nm;
  HashStringCsl.add all_licenses nm 
    {
      long_name = long_name;
      versions  = List.map OASISVersion.version_of_string versions;
      note      = note;
    };
  nm

let license_data () = 
  HashStringCsl.fold
    (fun license data acc -> 
       (license, 
        {data with 
             (* Really translate strings *)
             long_name = s_ data.long_name;
             note = 
               match data.note with 
                 | Some str -> Some (s_ str)
                 | None -> None})
       :: acc)
    all_licenses
    []

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

let bsd3 = 
  mk_license
    "BSD3"
    (ns_ "Berkeley software distribution license (3 clauses)")

let bsd4 = 
  mk_license
    "BSD4"
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
    failwithf1
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

let parse_rgxp =
  ignore "(*";
  Pcre.regexp ~flags:[`CASELESS] 
    "^(?<license>[A-Z0-9\\-]*?[A-Z0-9]+)\
     (-(?<version>[0-9\\.]+))?(?<later>\\+)?\
     ( *with *(?<exception>.*[^ ]+) *exception)?$"

let parse_dep5 ~ctxt str = 
  let substrs = 
    Pcre.exec ~rex:parse_rgxp str
  in
  let get_name nm = 
    Pcre.get_named_substring parse_rgxp nm substrs 
  in
  let license = 
    let res = 
      try 
        get_name "license"
      with Not_found ->
        failwithf1 
          (f_ "Undefined license in '%s'")
          str
    in
      if not (HashStringCsl.mem all_licenses res) then
        OASISMessage.warning ~ctxt
          (f_ "Unknown license '%s' in '%s'")
          res str;
      res
  in
  let version = 
    try 
      let ver = 
        OASISVersion.version_of_string (get_name "version")
      in
        try
          let _s : string = get_name "later" in
            VersionOrLater ver
        with Not_found ->
          Version ver
    with Not_found -> 
      NoVersion
  in
  let exceptions =
    try
      let res = 
        get_name "exception"
      in
        if HashStringCsl.mem all_exceptions res then
          [res]
        else
          failwithf2
            (f_ "Unknown license exception '%s' in '%s'")
            res str
    with Not_found -> 
      []
  in
    { 
      license    = license;
      exceptions = exceptions; 
      version    = version;
    }

let parse ~ctxt str = 
  try 
    begin
      DEP5License (parse_dep5 ~ctxt str)
    end
  with Not_found ->
    begin
      try 
        OtherLicense
          (OASISValues.url.parse ~ctxt str)
      with Failure _ ->
        failwithf1
          (f_ "Cannot parse license '%s'")
          str
    end


let to_string = 
  function
    | DEP5License t ->
        begin
          let ver = 
            match t.version with
              | Version v -> 
                  "-"^(OASISVersion.string_of_version v)
              | VersionOrLater v ->
                  "-"^(OASISVersion.string_of_version v)^"+"
              | NoVersion ->
                  ""
          in
          let exceptions = 
            match t.exceptions with
              | [] -> 
                  ""
              | lst ->
                  " with "^(String.concat ", " lst)^" exception"
          in
            t.license^ver^exceptions
        end

    | OtherLicense url ->
        url

let value =
  {
    parse  = parse;
    update = OASISValues.update_fail;
    print  = to_string;
  }


let help () =
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
    HashStringCsl.fold
      (fun license extra acc ->
         let dflt = 
           {license = license; version = NoVersion; exceptions = []}
         in
           List.fold_left
             (fun acc ver -> 
                let dflt = 
                  {dflt with version = Version ver}
                in
                  List.fold_left
                    (fun acc excpt ->
                       {dflt with exceptions = [excpt]} :: acc)
                    (* only versions *)
                    (dflt :: acc)
                    (exception_find license exceptions_map))
             (* only license *)
             (dflt :: acc)
             extra.versions)
      all_licenses
      []
  in
  let all = 
    List.rev_map (fun t -> DEP5License t) all 
  in
    (s_ "\n\

A correct license string follow the DEP-5 standard:

XXX-VVV(+)?( with OCaml linking exception)?

XXX: shortname of the license
VVV: optional version of the license, '+' means this 
version or later. XXX and VVV need to be consistent.

Recommended license: 

  LGPL-2.1 with OCaml linking exception

Available licenses: ")^
  (String.concat (s_ ", ")
     (List.rev_map to_string all))

                    
     
