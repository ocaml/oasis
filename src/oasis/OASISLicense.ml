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

type url = string with odn 

type license_exception = 
  | OCamlLinkingException
  | OtherException of url
  with odn

type t =
  | Proprietary
  | BSD3
  | BSD4
  | GPL
  | LGPL
  | QPL
  | CeCILL
  | CeCILLB
  | CeCILLC
  | LicenseWithVersion of t * OASISVersion.t
  | LicenseWithLaterVersion of t * OASISVersion.t
  | LicenseWithException of t * license_exception
  | OtherLicense of url
  with odn

(* END EXPORT *)

open OASISValues
open OASISGettext
open OASISUtils

let license_data () = 
  [
    Proprietary,
    "PROP",
    s_ "Proprietary license, all rights reserved",
    [];

    BSD3,
    "BSD3",
    s_ "Berkeley software distribution license (3 clauses)",
    [];

    BSD4,
    "BSD4",
    s_ "Berkeley software distribution license (3 clauses)",
    [];

    GPL,
    "GPL",
    s_ "GNU General Public License.",
    ["1.0"; "2.0"; "3.0"];

    LGPL,
    "LGPL",
    s_ "GNU Lesser General Public License, \
     (GNU Library General Public License for versions lower than 2.1).",
    ["2.0"; "2.1"; "3.0"];

    QPL,
    "QPL",
    s_ "Q Public License.",
    ["1.0"];

    CeCILL,
    "CeCILL",
    s_ "CEA-CNRS-INRIA Logiciel Libre, GPL-like",
    ["1"; "2"];

    CeCILLB,
    "CeCILL-B",
    s_ "CEA-CNRS-INRIA Logiciel Libre, BSD-like",
    [];

    CeCILLC,
    "CeCILL-C",
    s_ "CEA-CNRS-INRIA Logiciel Libre, LGPL-like",
    [];
  ]

let license_exception_data () =
  [
    OCamlLinkingException,
    s_ "OCaml linking",
    [LGPL];
  ]

let parse = 
  let rgxp_short_license =
    Str.regexp_case_fold "^ *\\([A-Z0-9\\-]*[A-Z0-9]+\\) *$"
  in

  let rgxp_version = 
    Str.regexp_case_fold "^ *\\(.*\\)-\\([0-9\\.]+\\)\\(+?\\) *$"
  in

  let rgxp_exception =
    Str.regexp_case_fold "^ *\\(.*\\) *with *\\(.*\\) *exception$"
  in

  let is_equal s1 s2 = 
    let no_whitespaces s =
      let buff =
        Buffer.create (String.length s)
      in
        String.iter
          (function 
             | ' ' -> ()
             | c -> Buffer.add_char buff c)
          s;
        Buffer.contents buff
    in
    let comparable_string s =
      String.lowercase (no_whitespaces s)
    in
      (comparable_string s1) = (comparable_string s2)
  in

  let rec parse_aux ~ctxt s =
    if Str.string_match rgxp_exception s 0 then
      begin
        let str_l =
          Str.matched_group 1 s
        in
        let excpt = 
          let str_excpt1 = 
            Str.matched_group 2 s
          in
            try 
              let excpt, _, _ =
                List.find 
                  (fun (_, str_excpt2, _) ->
                     is_equal str_excpt1 str_excpt2)
                  (license_exception_data ())
              in
                excpt
            with Not_found ->
              begin
                try 
                  OtherException 
                    (OASISValues.url.parse ~ctxt str_excpt1)
                with Failure _ -> 
                  failwithf1
                    (f_ "Cannot find license exception '%s'")
                    str_excpt1
              end
        in
          LicenseWithException (parse_aux ~ctxt str_l, excpt)
      end
    else if Str.string_match rgxp_version s 0 then
      begin
        let str_l =
          Str.matched_group 1 s
        in
        let str_ver =
          Str.matched_group 2 s
        in
        let and_later =
          (Str.matched_group 3 s) = "+" 
        in
        let l = 
          parse_aux ~ctxt str_l
        in
        let ver =
          OASISVersion.version_of_string str_ver
        in
          if and_later then
            LicenseWithLaterVersion(l, ver)
          else
            LicenseWithVersion(l, ver)
      end
    else if Str.string_match rgxp_short_license s 0 then
      begin
        let str_l1 =
          Str.matched_group 1 s
        in
        let l, _, _, _ =
          try 
            List.find 
              (fun (_, str_l2, _, _) ->
                 is_equal str_l1 str_l2)
              (license_data ())
          with Not_found -> 
            failwithf1
              (f_ "Cannot find license shortname '%s'")
              str_l1
        in
          l
      end
    else 
      begin
        try 
          OtherLicense
            (OASISValues.url.parse ~ctxt s)
        with Failure _ ->
          failwithf1
            (f_ "Cannot parse license '%s'")
            s
      end
  in
    parse_aux 
  

let rec to_string = 
  function
    | Proprietary
    | BSD3
    | BSD4
    | GPL
    | LGPL
    | QPL 
    | CeCILL
    | CeCILLB
    | CeCILLC as l1 ->
        begin
          try 
            let _, shrt, _, _=
              List.find  
                (fun (l2, _, _, _) -> l1 = l2)
                (license_data ())
            in
              shrt
          with Not_found ->
            failwith
              (s_ "Cannot find license short name")
        end
    | OtherLicense u ->
        u
    | LicenseWithLaterVersion (l, ver) ->
        (to_string l)^"-"^(OASISVersion.string_of_version ver)^"+"
    | LicenseWithVersion (l, ver) ->
        (to_string l)^"-"^(OASISVersion.string_of_version ver)
    | LicenseWithException (l, excpt1) ->
        begin 
          let str_excpt =
            match excpt1 with
              | OCamlLinkingException ->
                  begin 
                    let _, str, _ =
                      List.find
                        (fun (excpt2, _, _) -> excpt1 = excpt2)
                        (license_exception_data ())
                    in
                      str
                  end
              | OtherException u ->
                  u
          in
            to_string l^" with "^str_excpt^" exception"
        end
        

let value =
  {
    parse  = parse;
    update = OASISValues.update_fail;
    print  = to_string;
  }


let help () = 
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
     (List.map
        to_string
        (List.fold_left
           (fun acc ->
              function
                | e, _, _, [] ->
                    e :: acc
                | e, _, _, lst ->
                    List.fold_left
                      (fun acc v -> 
                         LicenseWithVersion 
                           (e, (OASISVersion.version_of_string v)) 
                         :: acc)
                      acc
                      lst)
           []
           (license_data ()))))


                    
     
