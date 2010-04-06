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

(** Test schema and generator
    @author Sylvain Le Gall
  *)

(* END EXPORT *)

open OASISTypes
open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext

let schema, generator =
  let schm =
   schema "Document"
  in
  let cmn_section_gen =
    OASISSection.section_fields (fun () -> (s_ "document")) schm
  in
  let build_tools = 
    OASISBuildSection.build_tools_field schm
  in
  let typ =
    new_field schm "Type"
      ~default:(OASISPlugin.builtin "none") 
      OASISPlugin.Test.value
      (fun () ->
         s_ "Plugin to use to build documentation.")
  in
  let custom =
    OASISCustom.add_fields schm ""
      (fun () -> s_ "Command to run before building the doc.")
      (fun () -> s_ "Command to run after building the doc.")
  in
  let title =
    new_field schm "Title"
      string_not_empty
      (fun () ->
         s_ "Title of the document.")
  in
  let authors =
    new_field schm "Authors"
      ~default:[]
      (comma_separated string_not_empty)
      (fun () ->
         s_ "Authors of the document.")
  in
  let abstract =
    new_field schm "Abstract"
      ~default:None
      (opt string_not_empty)
      (fun () ->
         s_ "Short paragraph giving an overview of the document.")
  in
  let doc_format =
    new_field schm "Format"
      ~default:OtherDoc
      (choices 
         (fun () -> "document format")
         ["HTML",       HTML "index.html";
          "Text",       DocText;
          "PDF",        PDF;
          "PostScript", PostScript;
          "Info",       Info "invalid.info";
          "DVI",        DVI;
          "Other",      OtherDoc])
      (fun () ->
         s_ "Format for the document.")
  in
  let index =
    new_field schm "Index"
      ~default:None
      (opt file)
      (fun () ->
         s_ "Index or top-level file for the document, only apply to \
             HTML and Info.")
  in
  let install_dir =
    new_field schm "InstallDir"
      ~default:None
      (opt (expand file))
      (fun () ->
         s_ "Default target directory to install data and documentation.")
  in
  let build, install, data_files = 
   OASISBuildSection.build_install_data_fields schm
  in
    schm,
    (fun nm data ->
       Doc
         (cmn_section_gen nm data,
          let doc_format =
            match doc_format data with 
              | HTML _ ->
                  begin
                    match index data with
                      | Some fn -> HTML fn
                      | None ->
                          failwithf1
                            (f_ "Index is mandatory for format HTML in \
                                 document %s")
                            nm
                  end
              | Info fn ->
                  begin
                    match index data with
                      | Some fn -> Info fn
                      | None ->
                          failwithf1
                            (f_ "Index is mandatory for format info in \
                                 document %s")
                            nm
                  end
              | DocText | PDF | PostScript | DVI | OtherDoc as fmt ->
                  fmt
          in
          let doc_install_dir =
            match install_dir data with
              | None ->
                  begin
                    match doc_format with
                      | HTML _     -> "$htmldir"
                      | DocText    -> "$docdir"
                      | PDF        -> "$pdfdir"
                      | PostScript -> "$psdir"
                      | Info _     -> "$infodir"
                      | DVI        -> "$dvidir"
                      | OtherDoc   -> "$docdir"
                  end
              | Some dir ->
                  dir
          in
            {
              doc_type        = typ data;
              doc_custom      = custom data;
              doc_build       = build data;
              doc_install     = install data;
              doc_install_dir = doc_install_dir;
              doc_title       = title data;
              doc_authors     = authors data;
              doc_abstract    = abstract data;
              doc_format      = doc_format;
              doc_data_files  = data_files data;
              doc_build_tools = build_tools data;
            }))
