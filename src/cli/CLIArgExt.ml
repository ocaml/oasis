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


(** Parsing of command line arguments
  *)


open OASISGettext
open BaseMessage
open CLISubCommand
open OASISUtils
open Format
open FormatExt
open FormatMarkdown


let global_options =
  ref []


let add_global_options lst =
  global_options := lst @ !global_options


let usage_msg =
  "OASIS v" ^ OASISVersion.string_of_version OASISConf.version_full
  ^ " (C) 2009-2010 OCamlCore SARL\n"
  ^ s_ "\n\
     oasis [global-options*] subcommand [subcommand-options*]\n\
     \n\
     Environment variables: \n\
     \n\
     OASIS_PAGER: pager to use to display long textual output.\n\
     \n\
     Global command line options:"


let specs () =
  [
    "-C",
    Arg.String (fun str -> Sys.chdir str),
    (s_ "dir Change directory before running.");
  ]
  @ (BaseContext.args ())
  @ !global_options


type help_extent =
  | NoSubCommand
  | SubCommand of string
  | AllSubCommand


type help_style =
  | Markdown
  | Output


let pp_print_help ignore_plugins hext hsty fmt () =

  (* Print with a precise length *)
  let pp_print_justified sz fmt str =
    let ns =
      String.make sz ' '
    in
      String.blit str 0 ns 0 (String.length str);
      pp_print_string fmt ns
  in

  (* Print definition for the output style *)
  let pp_print_output_def sz fmt (term, def) =
    pp_print_string fmt "  ";
    pp_print_justified sz fmt term;
    pp_print_string fmt "  ";
    pp_open_box fmt 0;
    pp_print_string_spaced fmt def;
    pp_close_box fmt ();
    pp_print_newline fmt ()
  in

  let pp_print_specs spec_help fmt specs =

    let help_specs =
      List.rev_append
        (List.rev_map
           (fun (cli, t, hlp) ->
              let arg, hlp =
                match OASISString.nsplit hlp ' ' with
                  | hd :: tl ->
                      hd, (String.concat " " tl)
                  | [] ->
                      "", ""
              in
              let arg =
                match t with
                  | Arg.Symbol (lst, _) ->
                      "{"^(String.concat "|" lst)^"}"
                  | _ ->
                      arg
              in
              let term =
                if arg <> "" then
                  cli^" "^arg
                else
                  cli
              in
                term, hlp)
           specs)
        (if spec_help then
           ["-help|--help", s_ "Display this list of options"]
         else
           [])
    in

    let sz =
      List.fold_left
        (fun acc (s, _) -> max (String.length s) acc)
        0
        help_specs
    in

    let pp_print_spec fmt (term, hlp) =
      match hsty with
        | Markdown ->
            pp_print_def fmt
              ("`"^term^"`")
              [pp_print_string_spaced, hlp]
        | Output ->
            pp_print_output_def
              sz fmt (term, hlp)
    in

      pp_print_list pp_print_spec "" fmt help_specs;
      if hsty = Output then
        pp_print_newline fmt ()
  in

  let pp_print_scmds fmt () =
    let scmds =
      List.map
        (fun scmd -> scmd.scmd_name, `Subcommand scmd)
        (CLISubCommand.list_builtin ())
    in
    let plugin_scmds =
      if not ignore_plugins then
        List.map
          (fun plugin -> plugin.PluginLoader.name, `Plugin plugin)
          (CLISubCommand.list_plugin ())
      else
        []
    in
    let all_scmds =
      List.sort
        (fun (nm1, _) (nm2, _) -> String.compare nm1 nm2)
        (List.rev_append scmds plugin_scmds)
    in
    let sz =
      (* Compute max size of the name. *)
      List.fold_left
        (fun sz (nm, c) ->
           max sz (String.length nm))
        0 all_scmds
    in
    let plugin_synopsis plg =
      match plg.PluginLoader.synopsis with
        | Some e -> e
        | None -> "No synopsis"
    in
    let plugin_markdown_data plg =
      let lst =
        match plg.PluginLoader.version with
          | Some v -> ["Version: "^v]
          | None -> []
      in
        (plugin_synopsis plg) ::
        ("Findlib: "^plg.PluginLoader.findlib_name) ::
        lst
    in
    let plugin_output_data plg =
      let findlib_name = plg.PluginLoader.findlib_name in
      let synopsis = plugin_synopsis plg in
      match plg.PluginLoader.version with
        | Some ver_str ->
            Printf.sprintf "%s (%s v%s)" synopsis findlib_name ver_str
        | None ->
            Printf.sprintf "%s (%s)" synopsis findlib_name
    in
      pp_print_para fmt (s_ "Available subcommands:");
      List.iter
        (fun (name, e) ->
           match hsty, e with
             | Markdown, `Subcommand scmd  ->
                 pp_print_def fmt ("`"^name^"`")
                   [pp_print_string_spaced, scmd.scmd_synopsis]
             | Markdown, `Plugin plg ->
                 pp_print_def fmt ("`"^name^"`")
                   (List.map
                      (fun s -> pp_print_string_spaced, s)
                      (plugin_markdown_data plg))
             | Output, `Subcommand scmd ->
                 pp_print_output_def
                   sz fmt (name, scmd.scmd_synopsis)
             | Output, `Plugin plg ->
                 pp_print_output_def
                   sz fmt
                   (name, plugin_output_data plg))
        all_scmds;
      if hsty = Output then
        pp_print_newline fmt ()
  in

  let pp_print_scmd fmt ~global_options scmd =
    pp_print_title 2 fmt
      (Printf.sprintf (f_ "Subcommand %s") scmd.scmd_name);

    pp_print_string fmt scmd.scmd_help;
    pp_print_endblock
      ~check_last_char:scmd.scmd_help
      fmt ();

    fprintf fmt
      (f_ "Usage: oasis [global-options*] %s %s")
      scmd.scmd_name scmd.scmd_usage;
    pp_print_endblock fmt ();

    if global_options then
      begin
        pp_print_para fmt (s_ "Global options: ");

        pp_print_specs true fmt (specs ())
      end;

    if scmd.scmd_specs <> [] then
      begin
        pp_print_para fmt (s_ "Options: ");

        pp_print_specs false fmt scmd.scmd_specs
      end
  in

    (* Write general introduction. *)
    begin
      match hext with
        | NoSubCommand | AllSubCommand ->
            begin
              pp_print_string fmt usage_msg;
              pp_print_endblock fmt ();

              pp_print_string fmt CLIData.main_mkd;
              pp_print_endblock
                ~check_last_char:CLIData.main_mkd
                fmt ();

              pp_print_specs true fmt (specs ());

              pp_print_scmds fmt ();
            end

        | SubCommand _ ->
            ()
    end;

    (* Write body, focusing on specific command selected. *)
    begin
      match hext with
        | NoSubCommand ->
            ()

        | SubCommand nm ->
            pp_print_scmd fmt
              ~global_options:true
              (CLISubCommand.find nm)

        | AllSubCommand ->
            List.iter
              (fun scmd ->
                 pp_print_scmd fmt ~global_options:false scmd)
              (CLISubCommand.list_builtin ())
    end


let parse () =
  let pos =
    ref 0
  in

  let scmd =
    ref
      (CLISubCommand.make
         (s_ "none")
         ""
         ""
         (fun () ->
            pp_print_help
              !CLICommon.ignore_plugins NoSubCommand Output err_formatter ();
            failwith
              (s_ "No subcommand defined, call 'oasis help' for help")))
  in

  let scmd_args =
    ref [||]
  in

  let set_scmd s =
    scmd := CLISubCommand.find s;

    (* Get the rest of arguments *)
    scmd_args :=
    Array.sub Sys.argv !pos ((Array.length Sys.argv) - !pos);

    (* Skip arguments *)
    pos := !pos + Array.length !scmd_args
  in

  let handle_error exc hext =
    let get_bad str =
      match split_newline ~trim:false str with
        | fst :: _ ->
            fst
        | [] ->
            s_ "Unknown error on the command line"
    in
      match exc with
        | Arg.Bad txt ->
            pp_print_help
              !CLICommon.ignore_plugins hext Output err_formatter ();
            prerr_newline ();
            prerr_endline (get_bad txt);
            exit 2
        | Arg.Help txt ->
            pp_print_help
              !CLICommon.ignore_plugins hext Output std_formatter ();
            exit 0
        | e ->
            raise e
  in

    (* Parse global options and set scmd *)
    begin
      try
        Arg.parse_argv
          ~current:pos
          Sys.argv
          (Arg.align (specs ()))
          set_scmd
          usage_msg
      with e ->
        handle_error e NoSubCommand
    end;

    (* Parse subcommand options *)
    begin
      try
        Arg.parse_argv
          ~current:(ref 0)
          !scmd_args
          (Arg.align !scmd.scmd_specs)
          !scmd.scmd_anon
          (Printf.sprintf
             (f_ "Subcommand %s options:\n")
             !scmd.scmd_name)
      with e ->
        handle_error e (SubCommand !scmd.scmd_name)
    end;

    !scmd.scmd_main
