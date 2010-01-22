
(** Helper function to write e _oasis file
    @author Sylvain Le Gall
  *)

open CommonGettext;;
open OASISSchema;;
open OASISTypes;;
open Format;;
open FormatExt;;

type 'a default =
  | Default_exists of 'a
  | Default_is of string
  | NoDefault
;;

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
              | "", _, Default_exists v ->
                  v
              | "", _, Default_is s ->
                  Printf.printf (f_ "Using default '%s'.\n") s;
                  parse s
              | s, _, _ ->
                  parse s
        end
      with Failure s -> 
        begin
          print_endline ("Error: "^s);
          ask_until_correct_aux ()
        end
    in
      input_until_decided ()
  in
    ask_until_correct_aux ()
;;

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
           failwith 
             (Printf.sprintf 
                "'%s' is not valid answer"
                s))
;;

let ask_yes_no ?help ?default q =
  ask_shortcut_choices 
    ?help 
    ?default
    q
    [
      (s_ "y"), (s_ "yes"), true;
      (s_ "n"), (s_ "no"),  false;
    ]
;;

let ask_field =
  ask_until_correct
;;

type section =
  | Library 
  | Executable
  | Flag
  | SourceRepo
  | Test
;;

let quickstart fmt lvl =
  let fake_context = 
    {
      OASISAstTypes.oasisfn = "";
      srcdir                = "";
      cond                  = None;
      valid_flags           = []
    }
  in

  let of_schema schema =
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
                           failwith
                             (Printf.sprintf 
                                (f_ "Trying to set '%s' using mandatory value '%s': %s")
                                key
                                s
                                (Printexc.to_string e))
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
                                   (Printf.sprintf (f_ "Value of %s?") key)
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
  in

  let pkg_data = 
    of_schema OASISPackage.schema
  in

  let sections =
    let section schema q_name =
      let nm = 
        ask_field q_name (fun s -> s)
      in
        nm, of_schema schema
    in

    let sections = 
      [
        s_ "n", s_ "stop", None;

        s_ "l", s_ "create a library",
        (Some 
           (fun () -> 
              Library,
              section
                OASISLibrary.schema
                (s_ "Library name?")));

        s_ "e", s_ "create an executable",
        (Some 
           (fun () ->
              Executable,
              section
                OASISExecutable.schema
                (s_ "Executable name?")));

        s_ "f", s_ "create a flag",
        (Some 
           (fun () ->
              Flag,
              section
                OASISFlag.schema
                (s_ "Flag name?")));

        s_ "s", s_ "create a source repository",
        (Some 
           (fun () ->
              SourceRepo,
              section
                OASISSourceRepository.schema
                (s_ "Source repository identifier?")));

        s_ "t", s_ "create a test",
        (Some 
           (fun () ->
              Test,
              section
                OASISTest.schema
                (s_ "Test name?")));
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
    let convert typ_ref gen =
      List.rev
        (List.fold_left
           (fun acc (typ, (nm, data)) ->
              if typ = typ_ref then
                (nm, gen nm data) :: acc
              else
                acc)
           []
           sections)
    in

      OASISPackage.generator 
        pkg_data 
        (convert Library OASISLibrary.generator)
        (convert Executable OASISExecutable.generator)
        (convert Flag OASISFlag.generator)
        (convert SourceRepo OASISSourceRepository.generator)
        (convert Test OASISTest.generator)
  in

  (** Pretty print the package 
    *)
  let pp_fields fmt (schm, data) = 
    let fake_data =
      PropList.Data.create ()
    in
    let key_value =
      List.rev
        (PropList.Schema.fold 
           (fun acc key extra _ ->
              try 
                let str =
                  PropList.Schema.get 
                    schm
                    data
                    key
                in
                let is_default =
                  try 
                    let default =
                      PropList.Schema.get 
                        schm
                        fake_data
                        key 
                    in
                      str = default
                  with 
                    | OASISValues.Not_printable 
                    | PropList.Not_set _ ->
                        (* Unable to compare so this is not default *)
                        false
                in
                  if not is_default then
                    (key, str) :: acc
                  else
                    acc
              with 
                | OASISValues.Not_printable ->
                    acc 
                | PropList.Not_set _  when extra.plugin <> None ->
                    acc)
           []
           schm)
    in

    let max_key_length =
      (* ":" *)
      1 
      +
      (* Maximum length of a key *)
      (List.fold_left
         max
         0

         (* Only consider length of key *)
         (List.rev_map
            fst

            (* Remove key/value that exceed line length *)
            (List.filter 
               (fun (k, v) -> k + v < pp_get_margin fmt ())
               
               (* Consider only length of key/value *)
               (List.rev_map
                  (fun (k, v) -> String.length k, String.length v)
                  key_value))))
    in

      pp_open_vbox fmt 0;
      List.iter
        (fun (k, v) ->
           pp_open_box fmt 2;
           pp_print_string fmt k;
           pp_print_string fmt ":";
           pp_print_break fmt (max 0 (max_key_length - String.length k)) 0;
           pp_print_string_spaced fmt v;
           pp_close_box fmt ();
           pp_print_cut fmt ())
        key_value;
      pp_close_box fmt ()
  in

  pp_open_vbox fmt 0;

  pp_fields fmt (OASISPackage.schema, pkg_data);
  pp_print_cut fmt ();

  List.iter 
    (fun (typ, (nm, data)) ->
       let typ_str, schm = 
         match typ with 
           | Library ->
               "Library", OASISLibrary.schema
           | Executable -> 
               "Executable", OASISExecutable.schema
           | SourceRepo ->
               "SourceRepository", OASISSourceRepository.schema
           | Test ->
               "Test", OASISTest.schema
           | Flag ->
               "Flag", OASISFlag.schema
       in
       let pp_id_or_string fmt id =
         let is_id str =
           let is_alpha c = 
             ('a' <= c && c <= 'z') ||  ('A' <= c && c <= 'Z')
           in
           let res =
             ref (String.length str > 0 && is_alpha str.[0])
           in
             String.iter
               (fun c -> res := !res && (is_alpha c || '0' <= c && c <= '9' || c = '_'))
               str;
             !res
         in
           if is_id id then 
             fprintf fmt "%s" id
           else 
             fprintf fmt "%S" id
       in
         fprintf fmt "@[<v 2>%s %a@,%a@]@,"
           typ_str
           pp_id_or_string nm
           pp_fields (schm, data))
    sections;

  pp_close_box fmt ()
;;

