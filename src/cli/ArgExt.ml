
(** Parsing of command line arguments 
  *)

open MainGettext
open OASISMessage
open SubCommand
open OASISUtils
open Format
open FormatExt
open FormatMarkdown

let specs, usage_msg = 
  [
    "-C",
    Arg.String (fun str -> Sys.chdir str),
    (s_ "dir Change directory before running.");
  ] 
  @ (OASISMessage.args ()),

  s_ "OASIS (C) 2009-2010 OCamlCore SARL\n\
      \n\
      OASIS [global-options*] subcommand [subcommand-options*]\n\
      \n\
      Global options:"

type help_extent =
  | NoSubCommand
  | SubCommand of string
  | AllSubCommand 

type help_style =
  | Markdown 
  | Output

let pp_print_help hext hsty fmt () = 

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
                match split ' ' hlp with
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

    let sz = 
      SubCommand.fold
        (fun c sz ->
           max sz (String.length c.scmd_name))
        0
    in
      pp_print_para fmt (s_ "Available subcommands:");

      SubCommand.fold
        (fun c () ->
           match hsty with 
             | Markdown ->
                 pp_print_def fmt ("`"^c.scmd_name^"`")
                   [pp_print_string_spaced, c.scmd_synopsis]
             | Output ->
                 pp_print_output_def 
                   sz fmt (c.scmd_name, c.scmd_synopsis))
        ();
      if hsty = Output then
        pp_print_newline fmt ()
  in

  let pp_print_scmd fmt scmd = 
    pp_print_title 2 fmt 
      (Printf.sprintf (f_ "Subcommand %s") scmd.scmd_name);
    
    pp_print_string fmt scmd.scmd_help;
    pp_print_endblock 
      ~check_last_char:scmd.scmd_help
      fmt ();

    fprintf fmt (f_ "Usage: OASIS [global-options*] %s %s") scmd.scmd_name scmd.scmd_usage;
    pp_print_endblock fmt ();

    if scmd.scmd_specs <> [] then
      begin
        pp_print_para fmt (s_ "Options: ");

        pp_print_specs false fmt scmd.scmd_specs
      end
  in

    pp_print_string fmt usage_msg;
    pp_print_endblock fmt ();

    pp_print_string fmt CLIData.main_mkd;
    pp_print_endblock 
      ~check_last_char:CLIData.main_mkd
      fmt ();

    pp_print_specs true fmt specs;

    pp_print_scmds fmt ();

    begin
      match hext with 
        | NoSubCommand ->
            ()
        | SubCommand nm ->
            pp_print_scmd fmt 
              (SubCommand.find nm)
        | AllSubCommand ->
            SubCommand.fold
              (fun scmd () ->
                 pp_print_scmd fmt scmd)
              ()
    end


let parse () = 
  let pos = 
    ref 0
  in

  let scmd = 
    ref 
      (SubCommand.make 
         (s_ "none")
         ""
         ""
         (fun () ->
            error "%s" 
              (s_ "No subcommand defined, call 'OASIS -help' for help")))
  in

  let scmd_args = 
    ref [||]
  in

  let set_scmd s =
    scmd := SubCommand.find s;

    (* Get the rest of arguments *)
    scmd_args := 
    Array.sub Sys.argv !pos ((Array.length Sys.argv) - !pos);
    
    (* Skip arguments *)
    pos := !pos + Array.length !scmd_args
  in

  let handle_error exc hext = 
    let get_bad str = 
      match split '\n' str with
        | fst :: _ ->
            fst
        | [] ->
            s_ "Unknown error on the command line"
    in
      match exc with 
        | Arg.Bad txt ->
            pp_print_help hext Output err_formatter ();
            prerr_newline ();
            prerr_endline (get_bad txt);
            exit 2
        | Arg.Help txt ->
            pp_print_help hext Output std_formatter ();
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
          (Arg.align specs)
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
