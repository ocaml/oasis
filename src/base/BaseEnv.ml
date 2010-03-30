
(** Read-only environment
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISGettext
open OASISUtils
open PropList

(** Origin of the variable, if a variable has been already set
    with a higher origin, it won't be set again
  *)
type origin_t = 
  | ODefault     (** Default computed value *)
  | OGetEnv      (** Extracted from environment, using Sys.getenv *)
  | OFileLoad    (** From loading file setup.data *)
  | OCommandLine (** Set on command line *)

(** Command line handling for variable 
  *)
type cli_handle_t =
  (** No command line argument *)
  | CLINone
  (** Build using variable name and help text *)
  | CLIAuto
   (** Use prefix --with- *)
  | CLIWith
  (** Use --enable/--disable *)
  | CLIEnable
  (** Fully define the command line arguments *)
  | CLIUser of (Arg.key * Arg.spec * Arg.doc) list

(** Variable type
  *)
type definition_t =
    {
      hide:       bool;
      dump:       bool;
      cli:        cli_handle_t;
      arg_help:   string option;
      group:      string option;
    }

(** Schema for environment 
  *)
let schema =
  Schema.create "environment"

(** Environment data 
  *)
let env = 
  Data.create ()

(** Lexer for var
  *)
let var_lxr = 
  Genlex.make_lexer []

(** Expand variable that can be found in string. Variable follow definition of
  * variable for {!Buffer.add_substitute}.
  *)
let rec var_expand str =
  let buff =
    Buffer.create ((String.length str) * 2)
  in
    Buffer.add_substitute 
      buff
      (fun var -> 
         let st =
           var_lxr (Stream.of_string var)
         in
           try 
             (* TODO: this is a quick hack to allow calling Test.Command 
              * without defining executable name really. I.e. if there is
              * an exec Executable toto, then $(toto) should be replace
              * by its real name. It is however useful to have this function
              * for other variable that depend on the host and should be 
              * written better than that.
              *)
             match Stream.npeek 3 st with 
               | [Genlex.Ident "utoh"; Genlex.Ident nm] ->
                   BaseFilePath.of_unix (var_get nm)
               | [Genlex.Ident "utoh"; Genlex.String s] ->
                   BaseFilePath.of_unix s
               | [Genlex.Ident "ocaml_escaped"; Genlex.Ident nm] ->
                   String.escaped (var_get nm)
               | [Genlex.Ident "ocaml_escaped"; Genlex.String s] ->
                   String.escaped s
               | [Genlex.Ident nm] ->
                   var_get nm
               | _ ->
                   failwithf2
                     (f_ "Unknown expression '%s' in variable expansion of %s.")
                     var
                     str
           with Unknown_field (_, _) ->
             failwithf2
               (f_ "No variable %s defined when trying to expand %S.")
               var 
               str)
      str;
    Buffer.contents buff

(** Get variable 
  *)
and var_get name =
  let vl = 
    Schema.get schema env name
  in
    var_expand vl

(** Choose a value among conditional expression
  *)
let var_choose lst =
  OASISExpr.choose 
    var_get 
    (fun et -> var_get (OASISExpr.string_of_expr_test et))
    lst

(** Protect a variable content, to avoid expansion
  *)
let var_protect vl = 
  let buff = 
    Buffer.create (String.length vl)
  in
    String.iter
      (function 
         | '$' -> Buffer.add_string buff "\\$"
         | c   -> Buffer.add_char   buff c)
      vl;
    Buffer.contents buff

(** Define a variable 
  *)
let var_define 
      ?(hide=false) 
      ?(dump=true) 
      ?short_desc
      ?(cli=CLINone)
      ?arg_help
      ?group 
      name
      dflt =

  let default =
    [
      ODefault, dflt;
      OGetEnv, lazy (Sys.getenv name);
    ]
  in

  let extra = 
    {
      hide     = hide;
      dump     = dump;
      cli      = cli;
      arg_help = arg_help;
      group    = group;
    }
  in

  (* Try to find a value that can be defined 
   *)
  let var_get_low lst = 
    let errors, res =
      List.fold_left
        (fun (errors, res) (_, v) ->
           if res = None then
             begin
               try 
                 errors, Some (Lazy.force v)
               with
                 | Not_found ->
                      errors, res
                 | Failure rsn ->
                     (rsn :: errors), res
                 | e ->
                     (Printexc.to_string e) :: errors, res
             end
           else
             errors, res)
        ([], None)
        (List.sort
           (fun (o1, _) (o2, _) ->
              if o1 < o2 then 
               1
              else if o1 = o2 then
                0
              else 
               -1)
           lst)
    in
      match res, errors with 
        | Some v, _ ->
            v
        | None, [] ->
            raise (Not_set (name, None))
        | None, lst ->
            raise (Not_set (name, Some (String.concat (s_ ", ") lst)))
  in

  let help =
    match short_desc with 
      | Some s -> Some (fun () -> s)
      | None -> None
  in

  let var_get_lst = 
    FieldRO.create
      ~schema
      ~name
      ~parse:(fun ?(context=ODefault) s -> [context, lazy s])
      ~print:var_get_low
      ~default
      ~update:(fun ?context x old_x -> x @ old_x)
      ?help
      extra
  in

    fun () ->
      var_expand (var_get_low (var_get_lst env))

(** Define a variable or redefine it
  *)
let var_redefine 
      ?hide
      ?dump
      ?short_desc
      ?cli
      ?arg_help
      ?group 
      name 
      dflt =
  if Schema.mem schema name then
    begin
      fun () -> 
        var_get name 
    end
  else
    begin
      var_define 
        ?hide
        ?dump
        ?short_desc
        ?cli
        ?arg_help
        ?group 
        name 
        dflt
    end

(** Well-typed ignore for var_define 
  *)
let var_ignore (e : unit -> string) = 
  ()

(** Display all variable 
  *)
let print_hidden =
  var_define 
    ~hide:true
    ~dump:false
    ~cli:CLIAuto
    ~arg_help:"Print even non-printable variable. (debug)"
    "print_hidden"
    (lazy "false")

(** Get all variable
  *)
let var_all () =
  List.rev
    (Schema.fold
       (fun acc nm def _ -> 
          if not def.hide || bool_of_string (print_hidden ()) then
            nm :: acc
          else
            acc)
       []
       schema)

(** Environment default file 
  *)
let default_filename =
  BaseEnvLight.default_filename

(** Initialize environment.
  *)
let load ?(allow_empty=false) ?(filename=default_filename) () =
  if Sys.file_exists filename then
    begin
      let chn =
        open_in_bin filename
      in
      let st =
        Stream.of_channel chn
      in
      let line =
        ref 1
      in
      let st_line = 
        Stream.from
          (fun _ ->
             try
               match Stream.next st with 
                 | '\n' -> incr line; Some '\n'
                 | c -> Some c
             with Stream.Failure -> None)
      in
      let lexer = 
        Genlex.make_lexer ["="] st_line
      in
      let rec read_file () =
        match Stream.npeek 3 lexer with 
          | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
              Stream.junk lexer; 
              Stream.junk lexer; 
              Stream.junk lexer;
              Schema.preset schema env nm ~context:OFileLoad value;
              read_file ()
          | [] ->
              ()
          | _ ->
              failwithf2
                (f_ "Malformed data file '%s' line %d")
                filename !line
      in
        read_file ();
        close_in chn
    end
  else if not allow_empty then
    begin
      failwithf1
        (f_ "Unable to load environment, the file '%s' doesn't exist.")
        filename
    end

(** Uninitialize environment 
  *)
let unload () = 
  (* TODO *)
  ()

(** Save environment on disk.
  *)
let dump ?(filename=default_filename) () = 
  let chn =
    open_out_bin filename
  in
    Schema.iter
      (fun nm def _ ->
         if def.dump then
           begin
             try 
               let value =
                 Schema.get 
                   schema 
                   env 
                   nm
               in
                 Printf.fprintf chn "%s = %S\n" nm value
             with Not_set _ ->
               ()
           end)
      schema;
    close_out chn

(** Display environment to user.
  *)
let print () =
  let printable_vars =
    Schema.fold
      (fun acc nm def short_descr_opt -> 
         if not def.hide || bool_of_string (print_hidden ()) then
           begin
             try 
               let value = 
                 Schema.get 
                   schema
                   env
                   nm
               in
               let txt = 
                 match short_descr_opt with 
                   | Some s -> s ()
                   | None -> nm
               in
                 (txt, value) :: acc
             with Not_set _ ->
                 acc
           end
         else
           acc)
      []
      schema
  in
  let max_length = 
    List.fold_left max 0
      (List.rev_map String.length
         (List.rev_map fst printable_vars))
  in
  let dot_pad str =
    String.make ((max_length - (String.length str)) + 3) '.'
  in

  print_newline ();
  print_endline "Configuration: ";
  print_newline ();
  List.iter 
    (fun (name,value) -> 
       Printf.printf "%s: %s %s\n" name (dot_pad name) value)
    printable_vars;
  Printf.printf "%!";
  print_newline ()

(** Default command line arguments 
  *)
let args () =
  let arg_concat =
    OASISUtils.varname_concat ~hyphen:'-'
  in
    [
      "--override",
       Arg.Tuple
         (
           let rvr = ref ""
           in
           let rvl = ref ""
           in
             [
               Arg.Set_string rvr;
               Arg.Set_string rvl;
               Arg.Unit 
                 (fun () -> 
                    Schema.set  
                      schema
                      env
                      ~context:OCommandLine 
                      !rvr
                      !rvl)
             ]
         ),
      "var+val  Override any configuration variable.";

    ]
    @
    List.flatten 
      (Schema.fold
        (fun acc name def short_descr_opt ->
           let var_set s = 
             Schema.set 
               schema
               env
               ~context:OCommandLine 
               name
               s
           in

           let arg_name = 
             OASISUtils.varname_of_string ~hyphen:'-' name
           in

           let hlp =
             match short_descr_opt with 
               | Some txt -> txt ()
               | None -> ""
           in

           let arg_hlp =
             match def.arg_help with 
               | Some s -> s
               | None   -> "str"
           in

           let default_value = 
             try 
               Printf.sprintf 
                 (f_ " [%s]")
                 (Schema.get
                    schema
                    env
                    name)
             with Not_set _ -> 
               ""
           in

           let args = 
             match def.cli with 
               | CLINone -> 
                   []
               | CLIAuto -> 
                   [
                     arg_concat "--" arg_name,
                     Arg.String var_set,
                     Printf.sprintf (f_ "%s %s%s") arg_hlp hlp default_value
                   ]
               | CLIWith ->
                   [
                     arg_concat "--with-" arg_name,
                     Arg.String var_set,
                     Printf.sprintf (f_ "%s %s%s") arg_hlp hlp default_value
                   ]
               | CLIEnable ->
                   [
                     arg_concat "--enable-" arg_name,
                     Arg.Unit (fun () -> var_set "true"),
                     Printf.sprintf (f_ " %s%s") hlp 
                       (if default_value = " [true]" then
                          (s_ " [default]")
                        else
                          "");

                     arg_concat "--disable-" arg_name,
                     Arg.Unit (fun () -> var_set "false"),
                     Printf.sprintf (f_ " %s%s") hlp 
                       (if default_value = " [false]" then
                          (s_ " [default]")
                        else
                          "");
                   ]
               | CLIUser lst ->
                   lst
           in
             args :: acc)
         []
         schema)
