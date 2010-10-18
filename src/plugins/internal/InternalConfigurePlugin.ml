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

(** Configure using internal scheme
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes
open OASISUtils
open OASISGettext
open BaseMessage

(** Configure build using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg argv =
  let var_ignore_eval var = 
    let _s : string =
      var ()
    in 
      ()
  in

  let errors = 
    ref SetString.empty
  in

  let buff =
    Buffer.create 13
  in

  let add_errors fmt =
    Printf.kbprintf
      (fun b ->
         errors := SetString.add (Buffer.contents b) !errors;
         Buffer.clear b)
      buff
      fmt
  in

  let warn_exception e =
    warning "%s" (string_of_exception e)
  in

  (* Check tools *)
  let check_tools lst =
    List.iter 
      (function
         | ExternalTool tool -> 
             begin
               try 
                 var_ignore_eval (BaseCheck.prog tool)
               with e ->
                 warn_exception e;
                 add_errors (f_ "Cannot find external tool '%s'") tool
             end
         | InternalExecutable nm1 ->
             (* Check that matching tool is built *)
             List.iter
               (function
                  | Executable ({cs_name = nm2}, 
                                {bs_build = build}, 
                                _) when nm1 = nm2 ->
                       if not (var_choose build) then
                         add_errors
                           (f_ "Cannot find buildable internal executable \
                                '%s' when checking build depends")
                           nm1
                  | _ ->
                      ())
               pkg.sections)
      lst
  in

  let build_checks sct bs =
    if var_choose bs.bs_build then
      begin
        if bs.bs_compiled_object = Native then
          begin
            try 
              var_ignore_eval BaseStandardVar.ocamlopt
            with e ->
              warn_exception e;
              add_errors
                (f_ "Section %s requires native compilation")
                (OASISSection.string_of_section sct)
          end;

        (* Check tools *)
        check_tools bs.bs_build_tools;

        (* Check depends *)
        List.iter  
          (function
             | FindlibPackage (findlib_pkg, version_comparator) ->
                 begin
                   try 
                     var_ignore_eval
                       (BaseCheck.package ?version_comparator findlib_pkg)
                   with e ->
                     warn_exception e;
                     match version_comparator with
                       | None ->
                           add_errors
                             (f_ "Cannot find findlib package %s")
                             findlib_pkg
                       | Some ver_cmp ->
                           add_errors
                             (f_ "Cannot find findlib package %s (%s)")
                             findlib_pkg
                             (OASISVersion.string_of_comparator ver_cmp)
                 end
             | InternalLibrary nm1 ->
                 (* Check that matching library is built *)
                 List.iter
                   (function
                      | Library ({cs_name = nm2},
                                 {bs_build = build}, 
                                 _) when nm1 = nm2 ->
                           if not (var_choose build) then
                             add_errors
                               (f_ "Cannot find buildable internal library \
                                    '%s' when checking build depends")
                               nm1
                      | _ ->
                          ())
                   pkg.sections)
          bs.bs_build_depends
      end
  in

  (* Parse command line *)
  BaseArgExt.parse argv (BaseEnv.args ());

  (* OCaml version *)
  begin
    match pkg.ocaml_version with 
      | Some ver_cmp ->
          begin
            try 
              var_ignore_eval
                (BaseCheck.version 
                   "ocaml" 
                   ver_cmp 
                   BaseStandardVar.ocaml_version)
            with e ->
              warn_exception e;
              add_errors 
                (f_ "OCaml version %s doesn't match version constraint %s")
                (BaseStandardVar.ocaml_version ())
                (OASISVersion.string_of_comparator ver_cmp)
          end
      | None ->
          ()
  end;
   
  (* Findlib version *)
  begin
    match pkg.findlib_version with 
      | Some ver_cmp ->
          begin
            try 
              var_ignore_eval
                (BaseCheck.version 
                   "findlib" 
                   ver_cmp 
                   BaseStandardVar.findlib_version)
            with e ->
              warn_exception e;
              add_errors 
                (f_ "Findlib version %s doesn't match version constraint %s")
                (BaseStandardVar.findlib_version ())
                (OASISVersion.string_of_comparator ver_cmp)
          end
      | None ->
          ()
  end;

  (* Check build depends *)
  List.iter
    (function
       | Executable (_, bs, _)
       | Library (_, bs, _) as sct ->
           build_checks sct bs
       | Doc (_, doc) ->
           if var_choose doc.doc_build then
             check_tools doc.doc_build_tools
       | Test (_, test) ->
           if var_choose test.test_run then
             check_tools test.test_tools
       | _ ->
           ())
    pkg.sections;

  (* Save and print environment *)
  if SetString.empty = !errors then
    begin
      dump ();
      print ()
    end
  else
    begin
      List.iter
        (fun e -> error "%s" e)
        (SetString.elements !errors);
      failwithf1
        (fn_ 
           "%d configuration error"
           "%d configuration errors"
           (SetString.cardinal !errors))
        (SetString.cardinal !errors)
    end

(* END EXPORT *)

open OASISPlugin
open InternalId

let plugin = 
  `Configure, name, Some version

let init () =
  let self_id, _ =
    Configure.create plugin
  in
  let doit ctxt pkg =  
    ctxt, 
    {
      chng_moduls    = [InternalData.internalsys_ml];
      chng_clean     = None;
      chng_distclean = None;
      chng_main = 
        (ODNFunc.func 
           configure 
           "InternalConfigurePlugin.configure");
    }
  in
    InternalId.init ();
    Configure.register_act self_id doit
