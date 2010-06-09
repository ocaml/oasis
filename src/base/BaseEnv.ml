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

(** Environment data from file
  *)
let env_from_file = 
  ref MapString.empty

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
         try 
           (* TODO: this is a quick hack to allow calling Test.Command 
            * without defining executable name really. I.e. if there is
            * an exec Executable toto, then $(toto) should be replace
            * by its real name. It is however useful to have this function
            * for other variable that depend on the host and should be 
            * written better than that.
            *)
           let st =
             var_lxr (Stream.of_string var)
           in
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
         with 
           | Unknown_field (_, _) ->
               failwithf2
                 (f_ "No variable %s defined when trying to expand %S.")
                 var 
                 str
           | Stream.Error e -> 
               failwithf3
                 (f_ "Syntax error when parsing '%s' when trying to \
                      expand %S: %s")
                 var
                 str
                 e)
      str;
    Buffer.contents buff

(** Get variable 
  *)
and var_get name =
  let vl = 
    try 
      Schema.get schema env name
    with Unknown_field _ as e ->
      begin
        try 
          MapString.find name !env_from_file
        with Not_found ->
          raise e
      end
  in
    var_expand vl

(** Choose a value among conditional expression
  *)
let var_choose ?printer ?name lst =
  OASISExpr.choose 
    ?printer
    ?name
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
      name (* TODO: type constraint on the fact that name must be a valid OCaml
                id *)
      dflt =

  let default =
    [
      OFileLoad, lazy (MapString.find name !env_from_file);
      ODefault,  dflt;
      OGetEnv,   lazy (Sys.getenv name);
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
      | Some fs -> Some fs
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
let load ?allow_empty ?filename () =
  env_from_file := BaseEnvLight.load ?allow_empty ?filename ()

(** Uninitialize environment 
  *)
let unload () = 
  (* TODO: reset lazy values *)
  env_from_file := MapString.empty;
  Data.clear env

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
