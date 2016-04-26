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


(** Helper function to write an _oasis file
    @author Sylvain Le Gall
*)


open OASISGettext
open OASISSchema_intern
open OASISTypes
open Format
open FormatExt
open OASISUtils
open OASISMessage


type 'a default =
  | Default_value of 'a
  | Default_answer of string
  | Default_not_printable
  | NoDefault


type interface =
  | Human
  | Machine


type 'a t =
  {
    id:        string;
    interface: interface;
    default:   'a default;
    help:      string;
    ctxt:      OASISContext.t;
    question:  string;
    parse:     string -> 'a;
  }


(** This expression is raised when an unprintable default
  * is chosen
*)
exception ChooseNotPrintableDefault


(** Convert a default into a text *)
let help_of_default =
  function
    | Default_answer dflt ->
      Printf.sprintf (f_ "Default is '%s'") dflt

    | Default_value _ | Default_not_printable ->
      s_ "A default exists, you can leave this field blank."

    | NoDefault ->
      s_ "No default exists, you need to answer this question."


(** Ask a question until a correct answer is given
*)
let ask_until_correct t =
  let rec ask_until_correct_aux () =
    let () =
      (* Question *)
      match t.interface with
        | Human ->
          Printf.printf "%s %!" t.question
        | Machine ->
          Printf.printf "???%s %!" t.id
    in
    try
      let answer =
(*
          try
            let _ =
              (* Check we have terminal *)
              Unix.tcgetattr Unix.stdin
            in
            (* TODO: use history file *)
            let q =
              Buffer.create 13
            in
            let rec input () =
              let c = Ledit.input_char stdin in
                if c = "\n" then
                  begin
                    Buffer.contents q
                  end
                else
                  begin
                    Buffer.add_string q c;
                    input ()
                  end
            in
              input ()

          with Unix.Unix_error _ ->
 *)
        (* Fallback if we don't have a terminal *)
        read_line ()
      in
      match answer, t.default with
        | "?", _ ->
          print_endline t.help;
          ask_until_correct_aux ()

        | "", Default_value v ->
          v

        | "", Default_answer s ->
          t.parse s

        | "", Default_not_printable ->
          raise ChooseNotPrintableDefault

        | s, _ ->
          t.parse s

    with
      | ChooseNotPrintableDefault as e ->
        begin
          raise e
        end

      | e ->
        begin
          Printf.printf (f_ "Error: %s\n") (Printexc.to_string e);
          begin
            match t.interface with
              | Human ->
                Printf.printf
                  (f_ "Answer '?' for help on this question.\n%!")
              | Machine ->
                ()
          end;
          ask_until_correct_aux ()
        end
  in
  let () =
    match t.interface with
      | Human ->
        print_newline ();
        print_endline t.help
      | Machine ->
        ()
  in
  ask_until_correct_aux ()


(** Try to transform a [Default_value _] into a
    [Default_answer _] if the value is among the choices
*)
let printable_default default choices =
  match default with
    | Default_answer _
    | NoDefault
    | Default_not_printable as d ->
      d

    | Default_value v as d ->
      begin
        try
          let chc, _ =
            List.find
              (fun (_, v') -> v = v')
              choices
          in
          Default_answer chc

        with Not_found ->
          d
      end


(** Extend [t] so that it can be processed using a choices
    list.
*)
let mk_shortcut_choices t choices =
  let help_choices =
    let fmt =
      str_formatter
    in
    pp_open_vbox fmt 0;
    pp_print_cut fmt ();
    pp_print_cut fmt ();
    pp_print_list
      (fun fmt (chc, nm, hlp, _) ->
         match hlp with
           | Some txt ->
             fprintf
               fmt
               "%s: @[%a, %a@]"
               chc
               pp_print_string_spaced nm
               pp_print_string_spaced txt
           | None ->
             fprintf
               fmt
               "%s: @[%a@]"
               chc
               pp_print_string_spaced nm)
      "@,"
      fmt
      choices;
    pp_print_cut fmt ();
    pp_close_box fmt ();
    flush_str_formatter ()
  in

  let choices_nohelp =
    List.map
      (fun (chc, _, _, vl) -> chc, vl)
      choices
  in

  let parse_shortcut s =
    List.assoc s choices_nohelp
  in

  (* The preprocessing we apply here, is that we also allow to use the name of
     choice. The difference is that you can give only a prefix of the name
     as long as it is not ambiguous. It also has lower priority than the
     shortcut.
  *)
  let name_choices =
    List.rev_map
      (fun (_, e, _, vl) -> e, vl)
      choices
  in

  let parse_name_choices s =
    (* [is_prefix pre str) Is [pre] a prefix of [str] *)
    let is_prefix pre (str, _) =
      if String.length str >= String.length pre then
        let pre_low =
          String.lowercase pre
        in
        let ans_low =
          String.lowercase (String.sub str 0 (String.length pre))
        in
        pre_low = ans_low
      else
        false
    in
    let res =
      List.filter
        (is_prefix s)
        name_choices
    in
    match res with
      | [_, uniq] ->
        uniq
      | _ ->
        raise Not_found
  in

  let parse s =
    try
      begin
        parse_shortcut s
      end

    with Not_found ->
      begin
        try
          parse_name_choices s
        with Not_found ->
          (* Fallback to original parsing *)
          t.parse s
      end
  in

  let default =
    printable_default t.default choices_nohelp
  in

  {t with
     help    = t.help^help_choices;
     default = default;
     parse   = parse}


let mk_numbered_choices t choices =
  let _, num_choices =
    List.fold_left
      (fun (i, acc) (e, hlp) -> (i + 1, (string_of_int i, e, hlp, e) :: acc))
      (1, [])
      choices
  in
  mk_shortcut_choices
    {t with help = t.help ^(s_ "\nChoices:")}
    (List.rev num_choices)


let mk_numbered_choices_multi t choices =
  let t =
    mk_numbered_choices t choices
  in

  let parse_one full s =
    try
      t.parse s
    with Failure _ ->
      failwithf
        (f_ "'%s' is not valid in answer '%s'")
        s full
  in

  let parse s =
    let lst =
      List.map
        (parse_one s)
        (* Split using " " and "," as separators *)
        (List.filter
           (fun s -> s <> "")
           (List.flatten
              (List.map
                 (fun s -> OASISString.split_comma s)
                 (OASISString.nsplit s ' '))))
    in
    String.concat ", " lst
  in

  {t with parse = parse}


let mk_yes_no t =
  mk_shortcut_choices
    t
    [
      (s_ "y"), (s_ "yes"), None, true;
      (s_ "n"), (s_ "no"),  None, false;
    ]


(** Ask questions for a schema (Package, Library, Executable...) *)
let ask_schema ~ctxt
    fields proplist_data features_data schema lvl interface plugins =
  let fake_context =
    {
      OASISAstTypes.cond = None;
      append             = false;
      valid_flags        = [];
      ctxt               = ctxt;
    }
  in

  let schm =
    schema.OASISSchema_intern.schm
  in

  let ask_field data key extra help =
    let default =
      try
        let s =
          PropList.Schema.get
            schm
            data
            key
        in
        Default_answer s
      with
        | OASISValues.Not_printable ->
          Default_not_printable

        | PropList.Not_set _ ->
          NoDefault
    in

    let has_default =
      default <> NoDefault
    in

    let set =
      PropList.Schema.set
        schm
        data
        key
        ~context:fake_context
    in
    begin
      match extra.qckstrt_lvl with
        | NoChoice s ->
          begin
            try
              set s
            with e ->
              failwithf
                (f_ "Trying to set field '%s' using mandatory \
                     value '%s': %s")
                key s (Printexc.to_string e)
          end
        | _ when not has_default || lvl >= extra.qckstrt_lvl ->
          begin
            let help =
              match help with
                | Some f ->
                  Printf.sprintf
                    (f_ "Field: %s\n%s\n%s\n")
                    key
                    (f ())
                    (help_of_default default)

                | None ->
                  Printf.sprintf
                    (f_ "Field: %s\n%s\n")
                    (help_of_default default)
                    key
            in
            let t =
              {
                id        = String.lowercase key;
                interface = interface;
                default   = default;
                help      = help;
                ctxt      = ctxt;
                question  = Printf.sprintf
                    (f_ "Value for field '%s'?")
                    key;
                parse     = (fun s -> set s; s);
              }
            in
            try
              begin
                match extra.qckstrt_q () with
                  | Field
                  | Text ->
                    begin
                      (* TODO: text *)
                      set (ask_until_correct t)
                    end

                  | Choices lst ->
                    begin
                      set
                        (ask_until_correct
                           (mk_numbered_choices_multi
                              t
                              (List.map (fun s -> s, None) lst)))
                    end

                  | ExclusiveChoices lst ->
                    begin
                      set
                        (ask_until_correct
                           (mk_numbered_choices
                              t
                              (List.map (fun s -> s, None) lst)))
                    end
              end
            with ChooseNotPrintableDefault ->
              ()
          end
        | _ ->
          ()
    end;
    data
  in

  let ask_field_and_get_answer data key extra help =
    let data =
      ask_field data key extra help
    in
    let str = PropList.Schema.get schm data key in
    data, str
  in

  let ask_plugin_fields plg data =
    PropList.Schema.fold
      (fun data key extra help ->
         match extra.kind with
           | FieldFromPlugin plg' ->
             begin
               if OASISPlugin.plugin_compare plg plg' = 0 then
                 ask_field data key extra help
               else
                 data
             end

           | StandardField | DefinePlugin _ | DefinePlugins _ ->
             begin
               data
             end)
      data
      schm
  in

  let ask (data, new_plugins) key extra help =
    match extra.kind with
      | StandardField ->
        begin
          ask_field data key extra help,
          new_plugins
        end

      | DefinePlugin knd ->
        begin
          let data, plg_str =
            ask_field_and_get_answer data key extra help
          in
          let plg =
            OASISPlugin.plugin_of_string knd plg_str
          in
          let data =
            ask_plugin_fields plg data
          in
          data, (OASISPlugin.SetPlugin.add plg new_plugins)
        end

      | DefinePlugins knd ->
        begin
          let data, plg_str =
            ask_field_and_get_answer data key extra help
          in
          List.fold_left
            (fun (data, new_plugins) plg ->
               ask_plugin_fields plg data,
               OASISPlugin.SetPlugin.add plg new_plugins)
            (data, new_plugins)
            (OASISPlugin.plugins_of_string knd plg_str)
        end

      | FieldFromPlugin plg ->
        begin
          (* We process only fields that have been defined
           * out of this loop, since plugins define in the
           * loop are processed after the plugin definition
          *)
          if OASISPlugin.SetPlugin.mem plg plugins then
            ask_field data key extra help, new_plugins
          else
            data, new_plugins
        end
  in

  let all_fields =
    PropList.Schema.fold
      (fun mp key extra help -> MapString.add key (extra, help) mp)
      MapString.empty schm
  in
  List.fold_left
    (fun (data, new_plugins) fld ->
       try
         let extra, help = MapString.find fld all_fields in
         match extra.feature with
           | Some ftr ->
             let features_data =
               OASISPlugin.SetPlugin.fold
                 (fun plg d -> OASISFeatures.Data.add_plugin plg d)
                 new_plugins features_data
             in
             if OASISFeatures.data_test ftr features_data then
               ask (data, new_plugins) fld extra help
             else
               data, new_plugins
           | None ->
             ask (data, new_plugins) fld extra help
       with Not_found ->
         failwithf
           (f_ "Unable to find field %s in schema %s.")
           fld (PropList.Schema.name schm))
    (proplist_data, plugins) fields


(** Ask questions for a package and its sections *)
let ask_package ~ctxt lvl intrf =
  let pkg_data, plugins, features_data =
    let first_fields =
      ["Name"; "Version"; "AlphaFeatures"; "BetaFeatures"]
    in
    let oasis_format = OASISConf.version_short in
    let other_fields =
      let first_fields_set = SetString.of_list first_fields in
      PropList.Schema.fold
        (fun acc key _ _ ->
           if SetString.mem key first_fields_set then
             acc
           else
             key :: acc)
        [] OASISPackage.schema.OASISSchema_intern.schm
    in
    let pkg_data_first, plugins_first =
      ask_schema ~ctxt first_fields
        (PropList.Data.create ())
        (OASISFeatures.Data.create oasis_format [] [])
        OASISPackage.schema lvl intrf
        OASISPlugin.SetPlugin.empty
    in
    let features_data_first =
      OASISFeatures.Data.create
        oasis_format
        (OASISPackage_intern.alpha_features pkg_data_first)
        (OASISPackage_intern.beta_features pkg_data_first)
    in
    let pkg_data, plugins =
      ask_schema ~ctxt other_fields
        pkg_data_first features_data_first
        OASISPackage.schema lvl intrf
        plugins_first
    in
    let features_data =
      OASISPlugin.SetPlugin.fold
        (fun plg d -> OASISFeatures.Data.add_plugin plg d)
        plugins features_data_first
    in
    pkg_data, plugins, features_data
  in

  let mk_t nm hlp q =
    {
      id        = nm;
      interface = intrf;
      default   = NoDefault;
      help      = hlp;
      ctxt      = ctxt;
      question  = q;
      parse     = fun s -> s;
    }
  in

  let format_string s =
    let fmt =
      str_formatter
    in
    pp_open_box fmt 0;
    pp_print_string_spaced fmt s;
    pp_close_box fmt ();
    flush_str_formatter ()
  in

  let section_data =
    let section gen schema q_name () =
      let nm =
        ask_until_correct (mk_t "name" "" q_name)
      in
      let fields =
        PropList.Schema.fold (fun acc key _ _ -> key :: acc) []
          schema.schm
      in
      (* Plugin defined in sections don't propagate *)
      let data, _ =
        ask_schema ~ctxt fields
          (PropList.Data.create ()) features_data
          schema lvl intrf plugins
      in
      gen features_data nm data
    in

    let sections =
      [
        s_ "n", s_ "stop", None, None;

        s_ "l", s_ "create a library", None,
        (Some
           (section
              OASISLibrary_intern.generator
              OASISLibrary.schema
              (s_ "Library name?")));

        s_ "e", s_ "create an executable", None,
        (Some
           (section
              OASISExecutable_intern.generator
              OASISExecutable.schema
              (s_ "Executable name?")));

        s_ "f", s_ "create a flag", None,
        (Some
           (section
              OASISFlag_intern.generator
              OASISFlag.schema
              (s_ "Flag name?")));

        s_ "s", s_ "create a source repository", None,
        (Some
           (section
              OASISSourceRepository_intern.generator
              OASISSourceRepository.schema
              (s_ "Source repository identifier?")));

        s_ "t", s_ "create a test", None,
        (Some
           (section
              OASISTest_intern.generator
              OASISTest.schema
              (s_ "Test name?")));

        s_ "d", s_ "create a document", None,
        (Some
           (section
              OASISDocument_intern.generator
              OASISDocument.schema
              (s_ "Document name?")));
      ]
    in

    let rec new_section acc hlp q =
      let next_opt =
        ask_until_correct
          (mk_shortcut_choices
             {(mk_t "create_section" hlp q)
              with
                 parse =
                   (fun s ->
                      failwithf
                        (f_ "'%s' is not a valid choice")
                        s);
                 default = Default_value None}
             sections)
      in
      match next_opt with
        | None ->
          List.rev acc
        | Some f ->
          new_section
            ((f ()) :: acc)
            (format_string
               (s_ "Section definition is complete. You can now \
                    now create additional sections:"))
            (s_ "Create another section?")

    in
    new_section []
      (format_string
         (s_ "General package definition is complete. You can now \
              create sections to describe various objects shipped \
              by this package:"))
      (s_ "Create a section?")
  in

  let pkg =
    (* Build the basic data structure *)
    OASISPackage_intern.generator
      pkg_data
      section_data
  in

  (* Apply completion from plugin *)
  let plugins =
    (* Standard plugins *)
    [
      (pkg.conf_type :> plugin_kind plugin);
      (pkg.build_type :> plugin_kind plugin);
      (pkg.install_type :> plugin_kind plugin);
    ]

    (* Extra plugins *)
    @ (pkg.plugins :> (plugin_kind plugin) list)

    (* Plugins from section *)
    @ (List.fold_left
        (fun lst sct ->
           match sct with
             | Executable _ | Library _ | Object _ ->
               lst
             | Test (cs, test) ->
               (test.test_type :> plugin_kind plugin) :: lst
             | Doc (cs, doc) ->
               (doc.doc_type :> plugin_kind plugin) :: lst
             | Flag _ | SrcRepo _ ->
               lst)
        [])
        pkg.sections
  in
  let pkg =
    List.fold_left
      (fun pkg plg -> (OASISPlugin.quickstart_completion plg) pkg)
      pkg
      plugins
  in
  pkg


(** Create an _oasis file *)
let to_file ~ctxt fn lvl intrf oasis_setup =

  let () =
    (* Print introduction *)
    let fmt =
      std_formatter
    in
    match intrf with
      | Human ->
        begin
          pp_open_vbox fmt 0;
          pp_open_box fmt 0;
          pp_print_string_spaced fmt
            (s_ "The program will ask some questions to create the \
                 `_oasis` file. If you answer '?' to a question, an help \
                 text will be displayed.");
          pp_close_box fmt ();
          pp_print_cut fmt ();
          pp_close_box fmt ();
          pp_print_flush fmt ();
        end

      | Machine ->
        ()

  in

  let () =
    if Sys.file_exists fn then
      begin
        let a =
          ask_until_correct
            (mk_yes_no
               {
                 id        = "overwrite";
                 interface = intrf;
                 default   = Default_value false;
                 help      = "";
                 ctxt      = ctxt;
                 parse     = bool_of_string;
                 question  =
                   Printf.sprintf
                     (f_ "File '%s' already exists, overwrite it?")
                     fn;
               })
        in
        if not a then
          failwithf
            (f_ "File '%s' already exists, remove it first")
            fn
      end
  in

  let pkg =
    ask_package ~ctxt lvl intrf
  in

  let content =
    let buf =
      Buffer.create 13
    in
    let fmt =
      formatter_of_buffer buf
    in
    OASISFormat.pp_print_package fmt pkg;
    Format.pp_print_flush fmt ();
    buf
  in

  let default_program env_var prg =
    try
      begin
        let prg =
          Sys.getenv env_var
        in
        info
          ~ctxt
          (f_ "Environment variable %s is set to '%s'") env_var prg;
        Some prg
      end
    with Not_found ->
      begin
        try
          begin
            let prg =
              OASISFileUtil.which ~ctxt prg
            in
            info
              ~ctxt
              (f_ "Program '%s' exists") prg;
            Some prg
          end
        with Not_found ->
          info
            ~ctxt
            (f_ "Environment variable %s is not set and program '%s' \
                 doesn't exists")
            env_var prg;
          None
      end
  in

  let cmd_exec prg fn =
    let cmd =
      (Filename.quote prg)
      ^" "^
        (Filename.quote fn)
    in
    info ~ctxt (f_ "Running command '%s'") cmd;
    match Sys.command cmd with
      | 0 -> ()
      | i ->
        failwithf
          (f_ "Command '%s' exited with status code %d")
          cmd
          i
  in

  let dump_tmp content =
    let tmp_fn, chn =
      Filename.open_temp_file fn ".tmp"
    in
    Buffer.output_buffer chn content;
    close_out chn;
    tmp_fn
  in

  let editor content =
    match default_program "EDITOR" "editor" with
      | Some prg ->
        begin
          let tmp_fn =
            dump_tmp content
          in
          begin
            try
              (* Edit content *)
              cmd_exec prg tmp_fn;

              (* Reload content *)
              begin
                let chn =
                  open_in tmp_fn
                in
                Buffer.clear content;
                Buffer.add_channel content chn (in_channel_length chn);
                close_in chn
              end;

              (* Remove temporary file *)
              Sys.remove tmp_fn

            with e ->
              Sys.remove tmp_fn;
              error ~ctxt "%s" (Printexc.to_string e)
          end
        end
      | None ->
        error ~ctxt "No way to edit the generated file."
  in

  let pager content =
    match default_program "PAGER" "pager" with
      | Some prg ->
        begin
          let tmp_fn =
            dump_tmp content
          in
          try
            cmd_exec prg tmp_fn;
            Sys.remove tmp_fn
          with e ->
            Sys.remove tmp_fn;
            error ~ctxt "%s" (Printexc.to_string e)
        end
      | None ->
        begin
          Buffer.output_buffer stdout content;
          flush stdout
        end
  in

  let create_fn content fn =
    let chn =
      open_out fn
    in
    info ~ctxt (f_ "Creating %s file\n%!") fn;
    Buffer.output_buffer chn content;
    close_out chn
  in

  let ask_end () =
    ask_until_correct
      (mk_shortcut_choices
         {
           id        = "end";
           interface = intrf;
           default   = NoDefault;
           help      = "Package definition is complete. Possible actions:";
           ctxt      = ctxt;
           question  = "What do you want to do now?";
           parse =
             (fun s ->
                failwithf
                  (f_ "'%s' is not a valid choice.")
                  s);
         }
         [
           s_ "d", s_ "display the generated file", None,
           (fun () ->
              (* Send to pager *)
              pager content;
              true);

           s_ "e", s_ "edit the generated file", None,
           (fun () ->
              (* Send to editor *)
              editor content;
              true);

           s_ "w", s_ "write and exit", None,
           (fun () ->
              create_fn content fn;
              false);

           s_ "r", s_ "write, run 'oasis setup' and exit", None,
           (fun () ->
              create_fn content fn;
              oasis_setup ();
              false);

           s_ "q", s_ "exit without saving", None,
           (fun () -> false);
         ])
  in

  while (ask_end ()) () do
    ()
  done
