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

(** Helper function to write an _oasis file
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISSchema
open OASISTypes
open Format
open FormatExt
open OASISUtils
open OASISMessage

type 'a default =
  | Default_exists of 'a
  | Default_is of string
  | NoDefault

let ask_until_correct q ?help ?(default=NoDefault) parse = 
  let rec ask_until_correct_aux () =
    let () = 
      (* Short introduction *)
      Printf.printf "\n%s " q;
      begin
        match default, help with 
          | Default_is dflt, Some _ ->
              Printf.printf (f_ "(default is '%s', type '?' for help) ") dflt
          | Default_exists _, Some _ ->
              Printf.printf (f_ "(default exists, type '?' for help) ") 
          | NoDefault, Some _ ->
              Printf.printf (f_ "(type '?' for help) ")

          | Default_is dflt, None ->
              Printf.printf (f_ "(default is '%s') ") dflt
          | Default_exists _, None ->
              Printf.printf (f_ "(default exists) ")
          | NoDefault, None ->
              ()
      end;
      Printf.printf "%!"
    in
    let rec input_until_decided () = 
      try
        begin
          let a = 
            read_line ()
          in
            match a, help, default with 
              | "?", Some hlp, _ ->
                  print_endline hlp;
                  flush stdout;
                  ask_until_correct_aux ()
              | "?", None, _ ->
                  print_endline (s_ "No help for this question.");
                  flush stdout;
                  ask_until_correct_aux ()
              | "", _, Default_exists v ->
                  v
              | "", _, Default_is s ->
                  Printf.printf (f_ "Using default '%s'.\n") s;
                  parse s
              | s, _, _ ->
                  parse s
        end
      with e -> 
        begin
          error ~exit:false "%s" (string_of_exception e);
          ask_until_correct_aux ()
        end
    in
      input_until_decided ()
  in
    ask_until_correct_aux ()

let ask_shortcut_choices ?help ?(default=NoDefault) q choices = 
  let help = 
    let fmt =
      str_formatter
    in
      pp_open_vbox fmt 0;
      begin
        match help with 
          | Some s -> 
              pp_print_string_spaced fmt s;
              pp_print_cut fmt ()
          | None ->
              ()
      end;
      pp_print_list
        (fun fmt (chc, hlp, _) ->
           fprintf 
             fmt
             "%s: @[%a@]"
             chc
             pp_print_string_spaced hlp)
        "@,"
        fmt
        choices;
      pp_close_box fmt ();

      Some (flush_str_formatter ())
  in

  let default =
    match default with 
      | Default_is _ | NoDefault as d ->
          d
      | Default_exists v as d ->
          begin
            try
              Default_is
                (List.assoc
                   v
                   (List.map 
                      (fun (chc, _, vl) -> vl, chc)
                      choices))
            with Not_found ->
              d
          end
  in
    ask_until_correct 
      ?help
      ~default
      q
      (fun s ->
         try 
           List.assoc
             s 
             (List.map (fun (c, _, v) -> (c, v)) choices)
         with Not_found ->
           failwithf1 (f_ "'%s' is not valid answer") s)

let ask_yes_no ?help ?default q =
  ask_shortcut_choices 
    ?help 
    ?default
    q
    [
      (s_ "y"), (s_ "yes"), true;
      (s_ "n"), (s_ "no"),  false;
    ]

(** Ask a single field *)
let ask_field =
  ask_until_correct

(** Ask questions for a schema (Package, Library, Executable...) *)
let ask_schema schema lvl =
  let fake_context = 
    {
      OASISAstTypes.cond = None;
      append             = false;
      valid_flags        = []
    }
  in

  PropList.Schema.fold
    (fun data key extra help -> 
       if extra.plugin = None then 
         begin
           let has_default, default = 
             try 
               let s = 
                 PropList.Schema.get
                   schema
                   data
                   key
               in
                 true, Default_is s 
             with 
               | OASISValues.Not_printable ->
                   (* Value exist but cannot be represented *)
                   true, Default_exists ()
               | PropList.Not_set _ ->
                   false, NoDefault
           in

           let parse = 
             PropList.Schema.set 
               schema
               data
               key
               ~context:fake_context
           in
             begin
               match extra.qckstrt_lvl with 
                 | NoChoice s -> 
                     begin
                       try
                         parse s
                       with e ->
                         failwithf3
                           (f_ "Trying to set field '%s' using mandatory value '%s': %s")
                           key s (Printexc.to_string e)
                     end
                 | _ when not has_default || lvl >= extra.qckstrt_lvl ->
                     begin
                       let help = 
                         match help with 
                           | Some f ->
                               Some (f ())
                           | None ->
                               None
                       in
                         match extra.qckstrt_q with 
                           | _ ->
                               (* TODO: handle other kind of questions *)
                               ask_field 
                                 (Printf.sprintf (f_ "Value for field '%s'?") key)
                                 ?help
                                 ~default
                                 parse
                     end
                 | _ ->
                     ()
             end;
             data
         end
       else
         begin
           data
         end)
    (PropList.Data.create ())
    schema

(** Ask questions for a package and its sections *)
let ask_package lvl = 
  let pkg_data = 
    ask_schema OASISPackage.schema lvl
  in

  let section_data =
    let section gen schema q_name =
      let nm = 
        ask_field q_name (fun s -> s)
      in
        gen nm (ask_schema schema lvl)
    in

    let sections = 
      [
        s_ "n", s_ "stop", None;

        s_ "l", s_ "create a library",
        (Some 
           (fun () -> 
              section
                OASISLibrary.generator
                OASISLibrary.schema
                (s_ "Library name?")));

        s_ "e", s_ "create an executable",
        (Some 
           (fun () ->
              section
                OASISExecutable.generator
                OASISExecutable.schema
                (s_ "Executable name?")));

        s_ "f", s_ "create a flag",
        (Some 
           (fun () ->
              section
                OASISFlag.generator
                OASISFlag.schema
                (s_ "Flag name?")));

        s_ "s", s_ "create a source repository",
        (Some 
           (fun () ->
              section
                OASISSourceRepository.generator
                OASISSourceRepository.schema
                (s_ "Source repository identifier?")));

        s_ "t", s_ "create a test",
        (Some 
           (fun () ->
              section
                OASISTest.generator
                OASISTest.schema
                (s_ "Test name?")));

        s_ "d", s_ "create a document",
        (Some
           (fun () ->
              section
                OASISDocument.generator
                OASISDocument.schema
                (s_ "Document name?")));
      ]
    in

    let rec new_section acc q = 
      match ask_shortcut_choices ~default:(Default_exists None) q sections with 
        | None ->
            List.rev acc
        | Some f ->
            new_section
              ((f ()) :: acc)
              (s_ "Create another section?")
    in
      new_section [] (s_ "Create a section?")
  in

  let _pkg = 
    (* Check that the global packaging is correct and get
     * order for package/library/test
     *)

    (* TODO: create a real function that do the check *)

    OASISPackage.generator 
      pkg_data 
      section_data 
  in

    pkg_data, section_data

(** Create an _oasis file *)
let to_file fn lvl =

  let () = 
    (* Print introduction *)
    let fmt =
      std_formatter
    in
      pp_open_box fmt 0;
      pp_print_string_spaced fmt
        (s_ "The program will ask some questions to create the OASIS file. \
             If you answer '?' to a question, an help text will be displayed.");
      pp_close_box fmt ();
      pp_print_flush fmt ();
      print_endline ""
  in

  let () = 
    if Sys.file_exists fn then
      begin
        let a = 
          ask_yes_no 
            ~default:(Default_exists false)
            (Printf.sprintf
               (f_ "File '%s' already exists, overwrite it?")
               fn)
        in
          if not a then
            failwithf1
              (f_ "File '%s' already exists, remove it first")
              fn
      end
  in

  let (pkg_data, section_data) = 
    ask_package lvl
  in

  let tmp_fn, chn = 
    Filename.open_temp_file fn ".tmp"
  in
  let () = 
    at_exit
      (fun () ->
         if Sys.file_exists tmp_fn then
           begin
             try
               FileUtil.rm [tmp_fn]
             with _ ->
               ()
           end)
  in

  let fmt = 
    Format.formatter_of_out_channel chn
  in
    info (f_ "Creating %s file\n%!") fn;
    OASISFormat.pp_print_package_proplist fmt (pkg_data, section_data);
    Format.pp_print_flush fmt ();
    close_out chn;
    if Sys.file_exists fn then
      FileUtil.rm [fn];
    FileUtil.mv tmp_fn fn
