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

  (* Check tools *)
  let check_tools lst =
    List.iter 
      (function
         | ExternalTool tool -> 
             var_ignore_eval (BaseCheck.prog tool)
         | InternalExecutable nm1 ->
             (* Check that matching tool is built *)
             List.iter
               (function
                  | Executable ({cs_name = nm2}, 
                                {bs_build = build}, 
                                _) when nm1 = nm2 ->
                       if not (var_choose build) then
                         failwithf1
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
            with PropList.Not_set _ ->
              failwithf1
                (f_ "Section %s requires native compilation")
                (OASISSection.string_of_section sct)
          end;

        (* Check tools *)
        check_tools bs.bs_build_tools;

        (* Check depends *)
        List.iter  
          (function
             | FindlibPackage (findlib_pkg, version_comparator) ->
                 var_ignore_eval
                   (BaseCheck.package ?version_comparator findlib_pkg)
             | InternalLibrary nm1 ->
                 (* Check that matching library is built *)
                 List.iter
                   (function
                      | Library ({cs_name = nm2},
                                 {bs_build = build}, 
                                 _) when nm1 = nm2 ->
                           if not (var_choose build) then
                             failwithf1 
                               (f_ "Cannot find buildable internal library \
                                    '%s' when checking build depends")
                               nm1
                      | _ ->
                          ())
                   pkg.sections)
          bs.bs_build_depends
      end
  in

  let ver_opt_check prefix std_var  =
    function
      | Some ver_cmp ->
          var_ignore_eval
            (BaseCheck.version prefix ver_cmp std_var)
      | None ->
          ()
  in


  (* Parse command line *)
  BaseArgExt.parse argv (args ());

  (* OCaml version *)
  ver_opt_check "ocaml" BaseStandardVar.ocaml_version pkg.ocaml_version;

  (* Findlib version *)
  ver_opt_check "findlib" BaseStandardVar.findlib_version pkg.findlib_version;

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
  dump ();
  print ()

(* END EXPORT *)

open OASISPlugin

let init () =
  let module PU = Configure.Make(InternalId)
  in
  let doit pkg =  
    {
      moduls       = [InternalData.internalsys_ml];
      setup        = ODNFunc.func 
                       configure 
                       "InternalConfigurePlugin.configure";
      clean        = None;
      distclean    = None;
      other_action = (fun _ -> ());
    },
    pkg
  in
    PU.register doit
