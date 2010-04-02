(* OASIS_START *)
(* DO NOT EDIT (digest: c320d468efb7b2310fc542d34d78054d) *)
module OASISGettext = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISGettext.ml"
  
  (** Gettext interface
    *)
  
  let s_ str = 
    str
  
  let f_ (str : ('a, 'b, 'c) format) =
    str
  
end

module OASISMessage = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISMessage.ml"
  
  (** Message to user
      @author Sylvain Le Gall
    *)
  
  open OASISGettext
  
  let verbose =
    ref true
  
  let debug =
    ref false
  
  (** Command line arguments
    *)
  let args () =
    ["-quiet",
     Arg.Clear verbose,
     (s_ " Run quietly");
  
     "-debug",
     Arg.Set debug,
     (s_ " Output debug message")]
  
  (**/**)
  let generic_message ?(after=ignore) cond beg fmt =
    if cond then
      begin
        Printf.fprintf stderr "%s: " beg;
        Printf.kfprintf 
          (fun chn -> 
             Printf.fprintf chn "\n%!";
             after ())
          stderr
          fmt
      end
    else
      begin
        Printf.ifprintf 
          stderr
          fmt
      end
  (**/**)
  
  
  (** Print a debug message
    *)
  let debug fmt =
    generic_message !debug "D" fmt
  
  (** Print information message.
    *)
  let info fmt = 
    generic_message !verbose "I" fmt
  
  (** Print a warning message 
    *)
  let warning fmt =
    generic_message !verbose "W" fmt
  
  (** Print an error message and exit.
    *)
  let error fmt =
    generic_message ~after:(fun () -> exit 1) !verbose "E" fmt
end

module PropList = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/PropList.ml"
  
  (** Property list 
      @author Sylvain Le Gall
    *)
  
  open OASISGettext
  
  type name_t = string
  
  let no_context f =
    fun ?context s -> f s
  
  exception Not_set of name_t * string option 
  exception No_printer of name_t
  exception Unknown_field of name_t * name_t
  
  let string_of_exception =
    function
      | Not_set (nm, Some rsn) ->
          Printf.sprintf (f_ "Field '%s' is not set: %s") nm rsn
      | Not_set (nm, None) ->
          Printf.sprintf (f_ "Field '%s' is not set") nm
      | No_printer nm ->
          Printf.sprintf (f_ "No default printer for value %s") nm
      | Unknown_field (nm, schm) ->
          Printf.sprintf (f_ "Field %s is not defined in schema %s") nm schm
      | e ->
          raise e
  
  module Data =
  struct
  
    type t = 
        (name_t, unit -> unit) Hashtbl.t
  
    let create () =
      Hashtbl.create 13
  
# 62 "/home/gildor/programmation/oasis/src/oasis/PropList.ml"
  end
  
  module Schema = 
  struct
  
    type ('ctxt, 'extra) value_t =
        {
          get:   Data.t -> string;
          set:   Data.t -> ?context:'ctxt -> string -> unit;
          help:  (unit -> string) option;
          extra: 'extra;
        }
  
    type ('ctxt, 'extra) t =
        {
          name:      name_t;
          fields:    (name_t, ('ctxt, 'extra) value_t) Hashtbl.t;
          presets:   (name_t, 'ctxt option * string) Hashtbl.t;
          order:     name_t Queue.t;
          name_norm: string -> string;
        }
  
    let create ?(case_insensitive=false) nm = 
      {
        name      = nm;
        fields    = Hashtbl.create 13;
        presets   = Hashtbl.create 13;
        order     = Queue.create ();
        name_norm = 
          (if case_insensitive then 
             String.lowercase
           else
             fun s -> s);
      }
  
    let add t nm set get extra help = 
      let key = 
        t.name_norm nm
      in
  
      (* If available, set preset values *)
      let update_preset data =
        if Hashtbl.mem t.presets nm then
          begin
            let context, v = 
              Hashtbl.find t.presets nm
            in
              Hashtbl.remove t.presets nm;
              set data ?context v
          end
      in
  
      (* Set preset value before any other *)
      let set data ?context x =
        update_preset data;
        set data ?context x
      in
  
      (* Before get, set preset value *)
      let get data =
        update_preset data;
        get data
      in
  
        if Hashtbl.mem t.fields key then
          failwith
            (Printf.sprintf 
               (f_ "Field '%s' is already defined in schema '%s'")
               nm t.name);
        Hashtbl.add 
          t.fields 
          key 
          {
            set   = set; 
            get   = get; 
            help  = help;
            extra = extra;
          };
        Queue.add nm t.order 
  
    let mem t nm =
      Hashtbl.mem t.fields nm 
  
    let find t nm = 
      try
        Hashtbl.find t.fields (t.name_norm nm)
      with Not_found ->
        raise (Unknown_field (nm, t.name))
  
    let get t data ?(preset=false) nm =
      try 
        (find t nm).get 
          data
      with Unknown_field _ as e when preset->
        begin
          try 
            snd (Hashtbl.find t.presets nm)
          with Not_found ->
            raise e
        end
  
    let set t data nm ?context x =
      (find t nm).set 
        data 
        ?context 
        x
  
    let preset t data nm ?context x = 
      Hashtbl.add t.presets nm (context, x)
  
    let fold f acc t =
      Queue.fold 
        (fun acc k ->
           let v =
             find t k
           in
             f acc k v.extra v.help)
        acc 
        t.order
  
    let iter f t =
      fold 
        (fun () -> f)
        ()
        t
  
  end
  
  module Field =
  struct
  
    type ('ctxt, 'value, 'extra) t =
        {
          set:    Data.t -> ?context:'ctxt -> 'value -> unit;
          get:    Data.t -> 'value;
          sets:   Data.t -> ?context:'ctxt -> string -> unit;
          gets:   Data.t -> string;
          help:   (unit -> string) option;
          extra:  'extra;
        }
  
    let new_id = 
      let last_id =
        ref 0
      in
        fun () -> incr last_id; !last_id
  
    let create ?schema ?name ?parse ?print ?default ?update ?help extra =
      (* Default value container *)
      let v = 
        ref None 
      in
  
      (* If name is not given, create unique one *)
      let nm = 
        match name with 
          | Some s -> s
          | None -> Printf.sprintf "_anon_%d" (new_id ())
      in
  
      (* Last chance to get a value: the default *)
      let default () = 
        match default with 
          | Some d -> d
          | None -> raise (Not_set (nm, Some (s_ "no default value")))
      in
  
      (* Get data *)
      let get data =
        (* Get value *)
        try 
          (Hashtbl.find data nm) ();
          match !v with 
            | Some x -> x 
            | None -> default ()
        with Not_found ->
          default ()
      in
  
      (* Set data *)
      let set data ?context x = 
        let x = 
          match update with 
            | Some f ->
                begin
                  try 
                    f ?context (get data) x
                  with Not_set _ ->
                    x
                end
            | None ->
                x
        in
          Hashtbl.replace 
            data 
            nm 
            (fun () -> v := Some x) 
      in
  
      (* Parse string value, if possible *)
      let parse =
        match parse with 
          | Some f -> 
              f
          | None ->
              fun ?context s ->
                failwith 
                  (Printf.sprintf 
                     (f_ "Cannot parse field '%s' when setting value %S")
                     nm
                     s)
      in
  
      (* Set data, from string *)
      let sets data ?context s =
        set ?context data (parse ?context s)
      in
  
      (* Output value as string, if possible *)
      let print =
        match print with
          | Some f ->
              f
          | None ->
              fun _ -> raise (No_printer nm)
      in
  
      (* Get data, as a string *)
      let gets data =
        print (get data)
      in
  
        begin 
          match schema with 
            | Some t ->
                Schema.add t nm sets gets extra help
            | None ->
                ()
        end;
  
        {
          set   = set;
          get   = get;
          sets  = sets;
          gets  = gets;
          help  = help;
          extra = extra;
        }
  
    let fset data t ?context x = 
      t.set data ?context x
  
    let fget data t =
      t.get data
  
    let fsets data t ?context s =
      t.sets data ?context s
  
    let fgets data t =
      t.gets data 
  
  end
  
  module FieldRO =
  struct
  
    let create ?schema ?name ?parse ?print ?default ?update ?help extra =
      let fld = 
        Field.create ?schema ?name ?parse ?print ?default ?update ?help extra
      in
        fun data -> Field.fget data fld
  
  end
end

module OASISTypes = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISTypes.ml"
  
  (** OASIS types and exceptions
     @author Sylvain Le Gall
    *)
  
  
  
  (** Alias type
    *)
  type name         = string 
  type package_name = string 
  type url          = string 
  type dirname      = string 
  type filename     = string 
  type prog         = string 
  type arg          = string 
  type args         = arg list 
  type command      = string 
  type command_line = (command * args) 
  
  (* Package name for findlib, doesn't contain '.' *)
  type findlib_name = string 
  (* Package path, made of several findlib name concatenated with '.' *)
  type findlib_path = string 
  
  (** Version 
    *)
  type version =
    | VInt of int * version
    | VNonInt of string * version
    | VEnd
    
  
  (** Version comparator
    *)
  type version_comparator = 
    | VGreater of version
    | VGreaterEqual of version
    | VEqual of version
    | VLesser of version
    | VLesserEqual of version
    | VOr of  version_comparator * version_comparator
    | VAnd of version_comparator * version_comparator
    
  
  (** Valid licenses
    *)
  type license =
    | AllRightsReserved
    | BSD3
    | BSD4
    | GPL
    | LGPL
    | LGPL_link_exn
    | PublicDomain
    | OtherLicense of url
    
  
  (** Compilation type
    *)
  type compiled_object =
    | Byte
    | Native
    | Best
    
  
  (** Package dependency
    *)
  type dependency = 
    | FindlibPackage of findlib_path * version_comparator option
    | InternalLibrary of name
    
  
  (** Tool dependency
    *)
  type tool =
    | ExternalTool of name
    | InternalExecutable of name 
    
  
  (** Possible VCS 
    *)
  type vcs = 
    | Darcs 
    | Git 
    | Svn 
    | Cvs 
    | Hg 
    | Bzr 
    | Arch 
    | Monotone
    | OtherVCS of url
    
  
  (** Available test 
    *)
  type expr_test = 
    | TOs_type
    | TSystem
    | TArchitecture
    | TCcomp_type
    | TOCaml_version
    
  
  (** Boolean expression to express condition on values
    *)
  type expr =
    | EBool of bool
    | ENot of expr
    | EAnd of expr * expr
    | EOr of expr * expr
    | EFlag of string
    | ETest of expr_test * string
    
  
  (** Conditional value
    *)
  type 'a conditional = (expr * 'a) list 
  
  type plugin = name * version option 
  
  type custom = 
      {
        pre_command:  (command_line option) conditional;
        post_command: (command_line option) conditional; 
      }
      
  
  type common_section =
      {
        cs_name: name;
        cs_data: PropList.Data.t;
      }
      
  
  type build_section =
      {
        bs_build:           bool conditional;
        bs_install:         bool conditional;
        bs_path:            dirname;
        bs_compiled_object: compiled_object;
        bs_build_depends:   dependency list;
        bs_build_tools:     tool list;
        bs_c_sources:       filename list;
        bs_data_files:      (filename * filename option) list;
        bs_ccopt:           args conditional;
        bs_cclib:           args conditional;
        bs_dlllib:          args conditional;
        bs_dllpath:         args conditional;
        bs_byteopt:         args conditional;
        bs_nativeopt:       args conditional;
      }
      
  
  (** Library definition 
    *)
  type library = 
      {
        lib_modules:            string list;
        lib_internal_modules:   string list;
        lib_findlib_parent:     findlib_name option;
        lib_findlib_name:       findlib_name option;
        lib_findlib_containers: findlib_name list;
      } 
  
  (** Executable definition 
    *)
  type executable = 
      {
        exec_custom:          bool;
        exec_main_is:         filename;
      } 
  
  (** Command line flag defintion 
    *)
  type flag = 
      {
        flag_description:  string option;
        flag_default:      bool conditional;
      } 
  
  (** Source repository definition
    *)
  type source_repository = 
      {
        src_repo_type:        vcs;
        src_repo_location:    url;
        src_repo_browser:     url option;
        src_repo_module:      string option;
        src_repo_branch:      string option;
        src_repo_tag:         string option;
        src_repo_subdir:      filename option;
      } 
  
  (** Test definition
    *)
  type test = 
      {
        test_type:               plugin;
        test_command:            command_line conditional;
        test_custom:             custom;
        test_working_directory:  filename option;
        test_run:                bool conditional;
        test_tools:              tool list;
      } 
  
  (** Documentation definition
    *)
  type doc =
      {
        doc_type:        plugin;
        doc_custom:      custom;
        doc_build:       bool conditional;
        doc_install:     bool conditional;
        doc_install_dir: filename;
        doc_data_files:  (filename * filename option) list;
        doc_build_tools: tool list;
      } 
  
  type section =
    | Library    of common_section * build_section * library
    | Executable of common_section * build_section * executable
    | Flag       of common_section * flag
    | SrcRepo    of common_section * source_repository
    | Test       of common_section * test
    | Doc        of common_section * doc
    
  
  (** OASIS file whole content
    *)
  type package = 
      {
        oasis_version:    version;
        ocaml_version:    version_comparator option;
        findlib_version:  version_comparator option;
        name:             package_name;
        version:          version;
        license:          license;
        license_file:     filename option;
        copyrights:       string list;
        maintainers:      string list;
        authors:          string list;
        homepage:         url option;
        synopsis:         string;
        description:      string option;
        categories:       url list;
  
        conf_type:        plugin;
        conf_custom:      custom;
  
        build_type:       plugin;
        build_custom:     custom;
  
        install_type:     plugin;
        install_custom:   custom;
        uninstall_custom: custom;
  
        clean_custom:     custom;
        distclean_custom: custom;
  
        files_ab:         filename list;
        sections:         section list;
        plugins:          plugin list;
        schema_data:      PropList.Data.t;
      } 
  
end

module OASISUtils = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISUtils.ml"
  
  (** Various utilities for OASIS.
    *)
  
  module MapString = Map.Make(String)
  
  (** Build a MapString with an association list 
    *)
  let map_string_of_assoc assoc =
    List.fold_left
      (fun acc (k, v) -> MapString.add k v acc)
      MapString.empty
      assoc
  
  (** Set for String 
    *)
  module SetString = Set.Make(String)
  
  (** Add a list to a SetString
    *)
  let set_string_add_list st lst =
    List.fold_left 
      (fun acc e -> SetString.add e acc)
      st
      lst
  
  (** Build a set out of list 
    *)
  let set_string_of_list =
    set_string_add_list
      SetString.empty
  
  (** Split a string, separator not included
    *)
  let split sep str =
    let str_len =
      String.length str
    in
    let rec split_aux acc pos =
      if pos < str_len then
        (
          let pos_sep = 
            try
              String.index_from str pos sep
            with Not_found ->
              str_len
          in
          let part = 
            String.sub str pos (pos_sep - pos) 
          in
          let acc = 
            part :: acc
          in
            if pos_sep >= str_len then
              (
                (* Nothing more in the string *)
                List.rev acc
              )
            else if pos_sep = (str_len - 1) then
              (
                (* String end with a separator *)
                List.rev ("" :: acc)
              )
            else
              (
                split_aux acc (pos_sep + 1)
              )
        )
      else
        (
          List.rev acc
        )
    in
      split_aux [] 0
  
  
  (** [varname_of_string ~hyphen:c s] Transform a string [s] into a variable name, 
      following this convention: no digit at the beginning, lowercase, only a-z
      and 0-9 chars. Whenever there is a problem, use an hyphen char.
    *)
  let varname_of_string ?(hyphen='_') s = 
    if String.length s = 0 then
      begin
        invalid_arg "varname_of_string" 
      end
    else
      begin
        let buff = 
          Buffer.create (String.length s)
        in
          (* Start with a _ if digit *)
          if '0' <= s.[0] && s.[0] <= '9' then
            Buffer.add_char buff hyphen;
  
          String.iter
            (fun c ->
               if ('a' <= c && c <= 'z') 
                 || 
                  ('A' <= c && c <= 'Z') 
                 || 
                  ('0' <= c && c <= '9') then
                 Buffer.add_char buff c
               else
                 Buffer.add_char buff hyphen)
            s;
  
          String.lowercase (Buffer.contents buff)
      end
  
  (** [varname_concat ~hyphen p s] Concat variable name, removing hyphen at end
      of [p] and at beginning of [s].
    *)
  let varname_concat ?(hyphen='_') p s = 
    let p = 
      let p_len =
        String.length p
      in
        if p_len > 0 && p.[p_len - 1] = hyphen then
          String.sub p 0 (p_len - 1)
        else
          p
    in
    let s = 
      let s_len =
        String.length s
      in
        if s_len > 0 && s.[0] = hyphen then
          String.sub s 1 (s_len - 1)
        else
          s
    in
      Printf.sprintf "%s%c%s" p hyphen s
  
  
  (** Fail with a format string, with 1 to 5 args.
      This is ugly but trying to use 
      Printf.ksprintf failwith, fails because the 
      return function should also be polymorphic
      but is constrained by the format types ->
      return is typed string and not 'a...
    *)
  let failwithf1 fmt a =
    failwith (Printf.sprintf fmt a)
  
  let failwithf2 fmt a b =
    failwith (Printf.sprintf fmt a b)
  
  let failwithf3 fmt a b c =
    failwith (Printf.sprintf fmt a b c)
  
  let failwithf4 fmt a b c d =
    failwith (Printf.sprintf fmt a b c d)
  
  let failwithf5 fmt a b c d e =
    failwith (Printf.sprintf fmt a b c d e)
  
end

module OASISUnixPath = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISUnixPath.ml"
  
  (** Manipulate Unix style path
      @author Sylvain Le Gall
    *)
  
  let current_dir_name = "."
  
  let parent_dir_name = ".."
  
  let concat f1 f2 = 
    if f1 = current_dir_name then
      f2
    else if f2 = current_dir_name then
      f1
    else
      f1^"/"^f2
  
  let make =
    function
      | hd :: tl ->
          List.fold_left
            (fun f p -> concat f p)
            hd
            tl
      | [] ->
          invalid_arg "OASISUnixPath.make"
  
  let dirname f =
    try
      String.sub f 0 (String.rindex f '/')
    with Not_found ->
      current_dir_name
  
  let basename f =
    try 
      let pos_start =
        (String.rindex f '/') + 1
      in
        String.sub f pos_start ((String.length f) - pos_start)
    with Not_found ->
      f
  
  let chop_extension f =
    try 
      let last_dot =
        String.rindex f '.'
      in
      let sub =
        String.sub f 0 last_dot
      in
        try 
          let last_slash =
            String.rindex f '/'
          in
            if last_slash < last_dot then
              sub
            else
              f
        with Not_found ->
          sub
  
    with Not_found ->
      f
  
end

module OASISVersion = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISVersion.ml"
  
  (** Version comparisons
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open OASISGettext
  
  (** Compare versions
    *)
  let rec version_compare v1 v2 =
    compare v1 v2
  
  (** Convert a string into a version
    *)
  let version_of_string str =
    let is_digit c =
      '0' <= c && c <= '9'
    in
  
    let str_len =
      String.length str
    in
  
    let buff =
      Buffer.create str_len
    in
  
    let rec extract_filter test start = 
      if start < str_len && test str.[start] then
        (
          Buffer.add_char buff str.[start];
          extract_filter test (start + 1)
        )
      else
        (
          let res =
            Buffer.contents buff
          in
            Buffer.clear buff;
            res, start
        )
    in
  
    let extract_int vpos =
      let str, vpos =
        extract_filter is_digit vpos
      in
        int_of_string str, vpos
    in
  
    let extract_non_int vpos =
      extract_filter 
        (fun c -> not (is_digit c)) 
        vpos
    in
  
    let rec parse_aux pos =
      if pos < str_len then
        begin
          if is_digit str.[pos] then
            begin
              let vl, end_pos =
                extract_int pos
              in
                VInt (vl, parse_aux end_pos)
            end
          else
            begin
              let vl, end_pos =
                extract_non_int pos
              in
                VNonInt (vl, parse_aux end_pos)
            end
        end
      else
        VEnd 
    in
  
    let rec compress =
      function
        | VInt (i, VNonInt(".", (VInt _ as tl))) ->
            VInt (i, compress tl)
        | VInt (i, tl) ->
            VInt (i, compress tl)
        | VNonInt (i, tl) ->
            VNonInt (i, compress tl)
        | VEnd ->
            VEnd
    in
  
      compress (parse_aux 0)
  
  (** Convert a version to a string
    *)
  let rec string_of_version =
    function
      | VInt (i, (VInt _ as tl)) ->
          (string_of_int i)^"."^(string_of_version tl)
      | VInt (i, tl) -> 
          (string_of_int i)^(string_of_version tl)
      | VNonInt (s, tl) -> 
          s^(string_of_version tl)
      | VEnd -> ""
  
  (** Apply version comparator expression
    *)
  let rec comparator_apply v op =
    match op with
      | VGreater cv ->
          (version_compare v cv) > 0
      | VGreaterEqual cv ->
          (version_compare v cv) >= 0
      | VLesser cv ->
          (version_compare v cv) < 0
      | VLesserEqual cv ->
          (version_compare v cv) <= 0
      | VEqual cv ->
          (version_compare v cv) = 0
      | VOr (op1, op2) ->
          (comparator_apply v op1) || (comparator_apply v op2)
      | VAnd (op1, op2) ->
          (comparator_apply v op1) && (comparator_apply v op2)
  
  (** Convert a comparator to string 
    *)
  let rec string_of_comparator =
    function 
      | VGreater v  -> "> "^(string_of_version v)
      | VEqual v    -> "= "^(string_of_version v)
      | VLesser v   -> "< "^(string_of_version v)
      | VGreaterEqual v -> ">= "^(string_of_version v)
      | VLesserEqual v  -> "<= "^(string_of_version v)
      | VOr (c1, c2)  -> 
          (string_of_comparator c1)^" || "^(string_of_comparator c2)
      | VAnd (c1, c2) -> 
          (string_of_comparator c1)^" && "^(string_of_comparator c2)
  
  (** Convert a comparator to a varname 
    *)
  let rec varname_of_comparator =
    let concat p v = 
      OASISUtils.varname_concat
        p 
        (OASISUtils.varname_of_string 
           (string_of_version v))
    in
      function 
        | VGreater v -> concat "gt" v
        | VLesser v  -> concat "lt" v
        | VEqual v   -> concat "eq" v
        | VGreaterEqual v -> concat "ge" v
        | VLesserEqual v  -> concat "le" v
        | VOr (c1, c2) ->
            (varname_of_comparator c1)^"_or_"^(varname_of_comparator c2)
        | VAnd (c1, c2) ->
            (varname_of_comparator c1)^"_and_"^(varname_of_comparator c2)
  
end

module OASISExpr = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISExpr.ml"
  
  (** OASIS expression manipulation
    *)
  
  open OASISTypes
  open OASISGettext
  open OASISUtils
  
  (** Evaluate each conditions and choose the right one. *)
  let choose ?printer var_get test_get lst =
    let rec eval =
      function
        | EBool b ->
            b
  
        | ENot e -> 
            not (eval e)
  
        | EAnd (e1, e2) ->
            (eval e1) && (eval e2)
  
        | EOr (e1, e2) -> 
            (eval e1) || (eval e2)
  
        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")
  
        | ETest (nm, vl) ->
            let v =
              test_get nm
            in
              (v = vl)
    in
  
    let rec choose_aux = 
      function
        | (cond, vl) :: tl ->
            if eval cond then 
              vl 
            else
              choose_aux tl
        | [] ->
            failwithf1
              (f_ "No result for a choice list: %s")
              (String.concat 
                 (s_ ", ")
                 (List.map
                    (fun (cond, vl) ->
                       match printer with
                         | Some p -> p vl
                         | None -> "<no printer>")
                    lst))
    in
      choose_aux (List.rev lst)
  
  (** All availbable expression tests and functions to convert it
      to string and reverse
    *)
  let expr_tests, string_of_expr_test, expr_test_of_string =
    let all =
      [
        TOs_type;
        TSystem;
        TArchitecture;
        TCcomp_type;
        TOCaml_version;
      ]
    in
    let to_string = 
      function 
        | TOs_type       -> "os_type"
        | TSystem        -> "system"
        | TArchitecture  -> "architecture"
        | TCcomp_type    -> "ccomp_type"
        | TOCaml_version -> "ocaml_version"
    in
    let of_string =
      let mp =
        List.rev_map (fun e -> to_string e, e) all
      in
        fun s -> 
          try 
            List.assoc (String.lowercase s) mp 
          with Not_found ->
            failwithf1 (f_ "Unknown OASIS test %s") s
    in
      all, to_string, of_string
  
end

module OASISSection = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISSection.ml"
  
  (** Manipulate section 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
  type section_kind =
    | KLibrary 
    | KExecutable
    | KFlag
    | KSrcRepo
    | KTest
    | KDoc
  
  (** Extract generic information 
    *)
  let section_kind_common = 
    function
      | Library (cs, _, _) -> 
          KLibrary, cs
      | Executable (cs, _, _) ->
          KExecutable, cs
      | Flag (cs, _) ->
          KFlag, cs
      | SrcRepo (cs, _) ->
          KSrcRepo, cs
      | Test (cs, _) ->
          KTest, cs
      | Doc (cs, _) ->
          KDoc, cs
  
  (** Common section of a section
    *)
  let section_common sct =
    snd (section_kind_common sct)
  
  (** Key used to identify section
    *)
  let section_id sct = 
    let k, cs = 
      section_kind_common sct
    in
      k, cs.cs_name
  
  let string_of_section sct =
    let k, nm =
      section_id sct
    in
      (match k with
         | KLibrary    -> "library" 
         | KExecutable -> "executable"
         | KFlag       -> "flag"
         | KSrcRepo    -> "src repository"
         | KTest       -> "test"
         | KDoc        -> "doc")
      ^" "^nm
  
end

module OASISBuildSection = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISBuildSection.ml"
  
  (** Build section functions
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISExecutable = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISExecutable.ml"
  
  (** Executable schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
  (* Return the name of the real name of executable, with full 
     unix path
   *)
  let unix_exec_is (cs, bs, exec) is_native ext_dll suffix_program = 
    let dir = 
      OASISUnixPath.concat
        bs.bs_path
        (OASISUnixPath.dirname exec.exec_main_is)
    in
    let is_native_exec = 
      match bs.bs_compiled_object with
        | Native -> true
        | Best -> is_native ()
        | Byte -> false
    in
  
      OASISUnixPath.concat
        dir
        (cs.cs_name^(suffix_program ())),
  
      if not is_native_exec && 
         not exec.exec_custom && 
         bs.bs_c_sources <> [] then
        Some (dir^"/dll"^cs.cs_name^(ext_dll ()))
      else
        None
  
  
end

module OASISLibrary = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISLibrary.ml"
  
  (** Library schema and generator 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open OASISUtils
  open OASISGettext
  
  (** Compute all files expected by a build of the library
    *)
  let generated_unix_files (cs, bs, lib) 
        source_file_exists is_native ext_lib ext_dll =  
    (* The headers that should be compiled along *)
    let headers = 
      List.fold_left
        (fun hdrs modul ->
           try 
             let base_fn = 
               List.find
                 (fun fn -> 
                    source_file_exists (fn^".ml") ||
                    source_file_exists (fn^".mli") ||
                    source_file_exists (fn^".mll") ||
                    source_file_exists (fn^".mly")) 
                 (List.map
                    (OASISUnixPath.concat bs.bs_path)
                    [modul;
                     String.uncapitalize modul;
                     String.capitalize modul])
             in
               (base_fn^".cmi") :: hdrs
           with Not_found ->
             OASISMessage.warning
               (f_ "Cannot find source file matching \
                    module '%s' in library %s")
               modul cs.cs_name;
               (OASISUnixPath.concat bs.bs_path (modul^".cmi")) 
               :: hdrs)
        []
        lib.lib_modules
    in
  
    let acc_nopath =
      []
    in
  
    (* Compute what libraries should be built *)
    let acc_nopath =
      let byte acc =
        (cs.cs_name^".cma") :: acc
      in
      let native acc =
        (cs.cs_name^".cmxa") :: (cs.cs_name^(ext_lib ())) :: acc
      in
        match bs.bs_compiled_object with 
          | Native ->
              byte (native acc_nopath)
          | Best when is_native () ->
              byte (native acc_nopath)
          | Byte | Best ->
              byte acc_nopath
    in
  
    (* Add C library to be built *)
    let acc_nopath = 
      if bs.bs_c_sources <> [] then
        begin
          ("lib"^cs.cs_name^(ext_lib ()))
          ::
          ("dll"^cs.cs_name^(ext_dll ()))
          ::
          acc_nopath
        end
      else
        acc_nopath
    in
  
      (* All the files generated *)
      List.rev_append
        (List.rev_map
           (OASISUnixPath.concat bs.bs_path)
           acc_nopath)
        headers
  
  
  (** Library group are organized in trees
    *)
  type group_t = 
    | Container of findlib_name * (group_t list)
    | Package of (findlib_name * 
                  common_section *
                  build_section * 
                  library * 
                  (group_t list))
  
  (** Compute groups of libraries, associate root libraries with 
      a tree of its children. A group of libraries is defined by 
      the fact that these libraries has a parental relationship 
      and must be isntalled together, with the same META file.
    *)
  let group_libs pkg =
    (** Associate a name with its children *)
    let children =
      List.fold_left
        (fun mp ->
           function
             | Library (cs, bs, lib) ->
                 begin
                   match lib.lib_findlib_parent with 
                     | Some p_nm ->
                         begin
                           let children =
                             try 
                               MapString.find p_nm mp
                             with Not_found ->
                               []
                           in
                             MapString.add p_nm ((cs, bs, lib) :: children) mp
                         end
                     | None ->
                         mp
                 end
             | _ ->
                 mp)
        MapString.empty
        pkg.sections
    in
  
    (* Compute findlib name of a single node *)
    let findlib_name (cs, _, lib) =
      match lib.lib_findlib_name with 
        | Some nm -> nm
        | None -> cs.cs_name
    in
  
    (** Build a package tree *)
    let rec tree_of_library containers ((cs, bs, lib) as acc) =
      match containers with
        | hd :: tl ->
            Container (hd, [tree_of_library tl acc])
        | [] ->
            (* TODO: allow merging containers with the same 
             * name 
             *)
            Package 
              (findlib_name acc, cs, bs, lib,
               (try 
                  List.rev_map 
                    (fun ((_, _, child_lib) as child_acc) ->
                       tree_of_library 
                         child_lib.lib_findlib_containers
                         child_acc)
                    (MapString.find cs.cs_name children)
                with Not_found ->
                  []))
    in
  
      (* TODO: check that libraries are unique *)
      List.fold_left
        (fun acc ->
           function
             | Library (cs, bs, lib) when lib.lib_findlib_parent = None -> 
                 (tree_of_library lib.lib_findlib_containers (cs, bs, lib)) :: acc
             | _ ->
                 acc)
        []
        pkg.sections
  
  (** Compute internal library findlib names, including subpackage
      and return a map of it.
    *)
  let findlib_name_map pkg = 
  
    (* Compute names in a tree *)
    let rec findlib_names_aux path mp grp =
      let fndlb_nm, children, mp =
        match grp with
          | Container (fndlb_nm, children) ->
              fndlb_nm, children, mp
                                    
          | Package (fndlb_nm, {cs_name = nm}, _, _, children) ->
              fndlb_nm, children, (MapString.add nm (path, fndlb_nm) mp)
      in
      let fndlb_nm_full =
        (match path with
           | Some pth -> pth^"."
           | None -> "")^
        fndlb_nm
      in
        List.fold_left
          (findlib_names_aux (Some fndlb_nm_full))
          mp
          children
    in
  
      List.fold_left
        (findlib_names_aux None)
        MapString.empty
        (group_libs pkg)
  
  
  (** Return the findlib name of the library without parents *)
  let findlib_of_name ?(recurse=false) map nm =
    try 
      let (path, fndlb_nm) = 
        MapString.find nm map
      in
        match path with 
          | Some pth when recurse -> pth^"."^fndlb_nm
          | _ -> fndlb_nm
  
    with Not_found ->
      failwithf1
        (f_ "Unable to translate internal library '%s' to findlib name")
        nm
  
  (** Return the findlib root name of a group, it takes into account
      containers. So the return group name is the toplevel name
      for both libraries and theirs containers.
    *)
  let findlib_of_group = 
    function
      | Container (fndlb_nm, _) 
      | Package (fndlb_nm, _, _, _, _) -> fndlb_nm
  
  (** Return the root library, i.e. the first found into the group tree
      that has no parent.
    *)
  let root_of_group grp =
    let rec root_lib_aux =
      function 
        | Container (_, children) ->
            root_lib_lst children        
        | Package (_, cs, bs, lib, children) ->
            if lib.lib_findlib_parent = None then 
              cs, bs, lib
            else
              root_lib_lst children
    and root_lib_lst =
      function
        | [] ->
            raise Not_found
        | hd :: tl ->
            try
              root_lib_aux hd
            with Not_found ->
              root_lib_lst tl
    in
      try
        root_lib_aux grp
      with Not_found ->
        failwithf1
          (f_ "Unable to determine root library of findlib library '%s'")
          (findlib_of_group grp)
  
end

module OASISFlag = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISFlag.ml"
  
  (** Flag schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISPackage = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISPackage.ml"
  
  (** Package schema and generator 
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISSourceRepository = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISSourceRepository.ml"
  
  (** SourceRepository schema and generator
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  
end

module OASISTest = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISTest.ml"
  
  (** Test schema and generator
      @author Sylvain Le Gall
    *)
  
end

module OASISDocumentation = struct
# 0 "/home/gildor/programmation/oasis/src/oasis/OASISDocumentation.ml"
  
  (** Test schema and generator
      @author Sylvain Le Gall
    *)
  
end


# 1581 "setup.ml"
module BaseEnvLight = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseEnvLight.ml"
  
  (** Simple environment, allowing only to read values
    *)
  
  module MapString = Map.Make(String)
  
  type t = string MapString.t
  
  (** Environment default file 
    *)
  let default_filename =
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      "setup.data"
  
  (** Load environment.
    *)
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let rmp =
          ref MapString.empty
        in
          begin
            try 
              while true do 
                let line = 
                  input_line chn
                in
                  Scanf.sscanf line "%s = %S" 
                    (fun nm vl -> rmp := MapString.add nm vl !rmp)
              done;
              ()
            with End_of_file ->
              close_in chn
          end;
          !rmp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith 
          (Printf.sprintf 
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end
  
  (** Get a variable that evaluate expression that can be found in it (see
      {!Buffer.add_substitute}.
    *)
  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute 
          buff
          (fun var -> 
             try 
               var_expand (MapString.find var env)
             with Not_found ->
               failwith 
                 (Printf.sprintf 
                    "No variable %s defined when trying to expand %S."
                    var 
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)
end


# 1663 "setup.ml"
module BaseFilePath = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseFilePath.ml"
  
  (** Manipulate filename
      @author Sylvain Le Gall
    *)
  
  open Filename
  
  module Unix = OASISUnixPath
  
  (** Concat elements of a path
    *)
  let make =
    function 
      | [] ->
          invalid_arg "BaseFilename.make"
      | hd :: tl ->
          List.fold_left Filename.concat hd tl
  
  (** Convert a unix filename into host filename
    *)
  let of_unix ufn =
    make
      (List.map
         (fun p ->
            if p = Unix.current_dir_name then
              current_dir_name
            else if p = Unix.parent_dir_name then
              parent_dir_name
            else
              p)
         (OASISUtils.split '/' ufn))
  
end

module BaseEnv = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseEnv.ml"
  
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
      Schema.get schema env ~preset:true name
    in
      var_expand vl
  
  (** Choose a value among conditional expression
    *)
  let var_choose ?printer lst =
    OASISExpr.choose 
      ?printer
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
end

module BaseExec = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseExec.ml"
  
  (** Running commands 
      @author Sylvain Le Gall
    *)
  
  open OASISGettext
  open OASISUtils
  
  (** Run a command 
    *)
  let run cmd args =
    let cmdline =
      String.concat " " (cmd :: args)
    in
      OASISMessage.info (f_ "Running command '%s'") cmdline;
      match Sys.command cmdline with 
        | 0 ->
            ()
        | i ->
            failwithf2
              (f_ "Command '%s' terminated with error code %d")
              cmdline i
  
  (** Run a command and returns its output
    *)
  let run_read_output cmd args =
    let fn = 
      Filename.temp_file "oasis-" ".txt"
    in
    let () = 
      try
        run cmd (args @ [">"; fn])
      with e ->
        Sys.remove fn;
        raise e
    in
    let chn =
      open_in fn
    in
    let routput =
      ref []
    in
      (
        try
          while true do 
            routput := (input_line chn) :: !routput
          done
        with End_of_file ->
          ()
      );
      close_in chn;
      Sys.remove fn;
      List.rev !routput
  
  (** Run a command and returns only first line 
    *)
  let run_read_one_line cmd args = 
    match run_read_output cmd args with 
      | [fst] -> 
          fst
      | lst -> 
          failwithf1
            (f_ "Command return unexpected output %S")
            (String.concat "\n" lst)
  
end

module BaseFileUtil = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseFileUtil.ml"
  
  (** {1 File operation (install, which...)
    *)
  
  open OASISGettext
  
  (** Find a file among all provided alternatives
    *)
  let find_file paths exts = 
  
    (* Cardinal product of two list *)
    let ( * ) lst1 lst2 = 
      List.flatten 
        (List.map 
           (fun a -> 
              List.map 
                (fun b -> a,b) 
                lst2) 
           lst1)
    in
  
    let rec combined_paths lst = 
      match lst with
        | p1 :: p2 :: tl ->
            let acc = 
              (List.map 
                 (fun (a,b) -> Filename.concat a b) 
                 (p1 * p2))
            in
              combined_paths (acc :: tl)
        | [e] ->
            e
        | [] ->
            []
    in
  
    let alternatives =
      List.map 
        (fun (p,e) -> 
           if String.length e > 0 && e.[0] <> '.' then
             p ^ "." ^ e
           else
             p ^ e) 
        ((combined_paths paths) * exts)
    in
      List.find 
        (fun fn ->
           OASISMessage.debug (f_ "Testing file existence '%s'") fn;
           Sys.file_exists fn)
        alternatives
  
  (** Find real filename of an executable
    *)
  let which prg =
    let path_sep =
      match Sys.os_type with 
        | "Win32" ->
            ';'
        | _ ->
            ':'
    in
    let path_lst =
      OASISUtils.split 
        path_sep 
        (Sys.getenv "PATH")
    in
    let exec_ext = 
      match Sys.os_type with 
        | "Win32" ->
            "" 
            :: 
            (OASISUtils.split 
               path_sep 
               (Sys.getenv "PATHEXT"))
        | _ ->
            [""]
    in
      find_file [path_lst; [prg]] exec_ext 
  
  (**/**)
  let q = Filename.quote
  (**/**)
  
  (** Copy a file 
    *)
  let cp src tgt = 
    BaseExec.run
      (match Sys.os_type with 
       | "Win32" -> "copy"
       | _ -> "cp")
      [q src; q tgt]
  
  (** Create a directory
    *)
  let mkdir tgt =
    BaseExec.run 
      (match Sys.os_type with 
         | "Win32" -> "md" 
         | _ -> "mkdir")
      [q tgt]
  
  (** Remove a directory
    *)
  let rmdir tgt =
    if Sys.readdir tgt = [||] then
      begin
        match Sys.os_type with 
          | "Win32" ->
              BaseExec.run "rd" [q tgt]
          | _ ->
              BaseExec.run "rm" ["-r"; q tgt]
      end
  
  (** Expand a filename containing '*.ext' into corresponding
      real files
    *)
  let glob fn = 
   let basename = 
     Filename.basename fn
   in
     if String.length basename >= 2 &&
        basename.[0] = '*' &&
        basename.[1] = '.' then
       begin
         let ext_len =
           (String.length basename) - 2
         in
         let ext = 
           String.sub basename 2 ext_len
         in
         let dirname =
           Filename.dirname fn
         in
           Array.fold_left
             (fun acc fn ->
                try 
                  let fn_ext = 
                    String.sub 
                      fn 
                      ((String.length fn) - ext_len) 
                      ext_len
                  in
                    if fn_ext = ext then
                      (Filename.concat dirname fn) :: acc
                    else
                      acc
                with Invalid_argument _ ->
                  acc)
             []
             (Sys.readdir dirname)
       end
     else
       begin
         if Sys.file_exists fn then
           [fn]
         else
           []
       end
end

module BaseArgExt = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseArgExt.ml"
  
  (** Handle command line argument
      @author Sylvain Le Gall
    *)
  
  open OASISUtils
  open OASISGettext
  
  let parse argv args =
      (* Simulate command line for Arg *)
      let current =
        ref 0
      in
  
        try
          Arg.parse_argv
            ~current:current
            (Array.concat [[|"none"|]; argv])
            (Arg.align args)
            (failwithf1 (f_ "Don't know what to do with arguments: '%s'"))
            (s_ "configure options:")
        with 
          | Arg.Help txt ->
              print_endline txt;
              exit 0
          | Arg.Bad txt ->
              prerr_endline txt;
              exit 1
end

module BaseCheck = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseCheck.ml"
  
  (** {1 Checking for particular features} 
    *)
  
  open BaseEnv
  open OASISUtils
  open OASISGettext
  
  (** Look for a program among a list of alternative program
    * the first found is returned. 
    *)
  let prog_best prg prg_lst =
    var_redefine
      prg 
      (lazy 
         (let alternate = 
            List.fold_left 
              (fun res e ->
                 match res with 
                   | Some _ -> 
                       res
                   | None ->
                       try
                         Some (BaseFileUtil.which e)
                       with Not_found ->
                         None)
              None
              prg_lst
          in
            match alternate with
              | Some prg -> prg
              | None -> raise Not_found))
  
  (** Check the presence of a particular program.
    *)
  let prog prg =
    prog_best prg [prg]
  
  (** Check the presence of a program or its native version
    *)
  let prog_opt prg = 
    prog_best prg [prg^".opt"; prg]
  
  let ocamlfind = 
    prog "ocamlfind"
  
  (** Check version, following Sys.ocaml_version convention
    *)
  let version 
        var_prefix 
        cmp
        fversion 
        () = 
    (* Really compare version provided *)
    let var = 
      var_prefix^"_version_"^(OASISVersion.varname_of_comparator cmp)
    in
      var_redefine 
        ~hide:true 
        var
        (lazy
           (let version_str =
              match fversion () with 
                | "[Distributed with OCaml]" ->
                    begin
                      try 
                        (var_get "ocaml_version")
                      with Not_found ->
                        OASISMessage.warning 
                          "Variable ocaml_version not defined, fallback to default";
                        Sys.ocaml_version
                    end
                | res ->
                    res
            in
            let version =
              OASISVersion.version_of_string version_str
            in
              if OASISVersion.comparator_apply version cmp then
                version_str
              else
                failwithf3
                  (f_ "Cannot satisfy version constraint on %s: %s (version: %s)")
                  var_prefix
                  (OASISVersion.string_of_comparator cmp)
                  version_str))
        ()
  
  (** Get findlib package version 
    *)
  let package_version pkg =
    BaseExec.run_read_one_line 
      (ocamlfind ())
      ["query"; "-format"; "%v"; pkg]
  
  (** Check for findlib package
    *)
  let package ?version_comparator pkg () =
    let var =
      OASISUtils.varname_concat 
        "pkg_" 
        (OASISUtils.varname_of_string pkg)
    in
    let findlib_dir pkg = 
      let dir = 
        BaseExec.run_read_one_line
          (ocamlfind ())
          ["query"; "-format"; "%d"; pkg]
      in
        if Sys.file_exists dir && Sys.is_directory dir then
          dir
        else
          failwithf2
            (f_ "When looking for findlib package %s, \
                 directory %s return doesn't exist")
            pkg dir
    in
    let vl =
      var_redefine
        var
        (lazy (findlib_dir pkg))
        ()
    in
      (
        match version_comparator with 
          | Some ver_cmp ->
              ignore
                (version 
                   var
                   ver_cmp
                   (fun _ -> package_version pkg)
                   ())
          | None -> 
              ()
      );
      vl
end

module BaseOCamlcConfig = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseOCamlcConfig.ml"
  
  (** Read output of command ocamlc -config and transform it
    * into enviornment variable
    *)
  
  open BaseEnv
  open OASISUtils
  open OASISGettext
  
  module SMap = Map.Make(String)
  
  let ocamlc = 
    BaseCheck.prog_opt "ocamlc"
  
  let ocamlc_config_map =
    (* Map name to value for ocamlc -config output 
       (name ^": "^value) 
     *)
    let rec split_field mp lst = 
      match lst with 
        | line :: tl ->
            let mp =
              try
                let pos_semicolon =
                  String.index line ':'
                in
                  if pos_semicolon > 1 then            
                    (
                      let name =
                        String.sub line 0 pos_semicolon 
                      in
                      let linelen =
                        String.length line
                      in
                      let value =
                        if linelen > pos_semicolon + 2 then
                          String.sub 
                            line 
                            (pos_semicolon + 2) 
                            (linelen - pos_semicolon - 2)
                        else
                          ""
                      in
                        SMap.add name value mp
                    )
                  else
                    (
                      mp
                    )
              with Not_found ->
                (
                  mp
                )
            in
              split_field mp tl
        | [] ->
            mp
    in
  
      var_redefine
        "ocamlc_config_map"
        ~hide:true
        ~dump:false
        (lazy
           (var_protect
              (Marshal.to_string
                 (split_field 
                    SMap.empty
                    (BaseExec.run_read_output 
                       (ocamlc ()) ["-config"]))
                 [])))
  
  let var_define nm =
    (* Extract data from ocamlc -config *)
    let avlbl_config_get () = 
      Marshal.from_string 
        (ocamlc_config_map ())
        0
    in
    let nm_config =
      match nm with 
        | "ocaml_version" -> "version"
        | _ -> nm
    in
      var_redefine
        nm 
        (lazy
          (try
              let map =
                avlbl_config_get ()
              in
              let value = 
                SMap.find nm_config map
              in
                value
            with Not_found ->
              failwithf2
                (f_ "Cannot find field '%s' in '%s -config' output")
                nm
                (ocamlc ())))
  
end

module BaseStandardVar = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseStandardVar.ml"
  
  (** Most standard variables for OCaml 
      @author Sylvain Le Gall
    *)
  
  open OASISGettext
  open OASISTypes
  open BaseCheck
  open BaseEnv
  
  (** {2 Programs} *)
  
  let ocamlfind  = BaseCheck.ocamlfind
  let ocamlc     = BaseOCamlcConfig.ocamlc
  let ocamlopt   = prog_opt "ocamlopt"
  let ocamlbuild = prog "ocamlbuild"
  
  (** {2 Variables from OASIS package} 
    *)
  
  (**/**)
  let rpkg = 
    ref None
  
  let pkg_get () =
    match !rpkg with 
      | Some pkg -> pkg
      | None -> failwith (s_ "OASIS Package is not set")
  (**/**)
  
  let pkg_name = 
    var_define
      ~short_desc:(fun () -> s_ "Package name")
      "pkg_name"
      (lazy (fst (pkg_get ())))
  
  let pkg_version =
    var_define
      ~short_desc:(fun () -> s_ "Package version")
      "pkg_version"
      (lazy 
         (OASISVersion.string_of_version 
            (snd (pkg_get ()))))
  
  
  (** {2 OCaml config variable} *) 
  
  let c = BaseOCamlcConfig.var_define 
  let stdc et = BaseOCamlcConfig.var_define (OASISExpr.string_of_expr_test et)
  
  let os_type        = stdc TOs_type
  let system         = stdc TSystem
  let architecture   = stdc TArchitecture
  let ccomp_type     = stdc TCcomp_type
  let ocaml_version  = stdc TOCaml_version
  
  (* Check variable presence *)
  let () = 
    if false then 
      let v_of_et =
        function
          | TOs_type       -> os_type        
          | TSystem        -> system         
          | TArchitecture  -> architecture   
          | TCcomp_type    -> ccomp_type     
          | TOCaml_version -> ocaml_version  
      in
      let _lst : 'a list =
        List.map v_of_et OASISExpr.expr_tests
      in 
        ()
  
  let standard_library_default = c "standard_library_default"
  let standard_library         = c "standard_library"
  let standard_runtime         = c "standard_runtime"
  let bytecomp_c_compiler      = c "bytecomp_c_compiler"
  let native_c_compiler        = c "native_c_compiler"
  let model                    = c "model"
  let ext_obj                  = c "ext_obj"
  let ext_asm                  = c "ext_asm"
  let ext_lib                  = c "ext_lib"
  let ext_dll                  = c "ext_dll"
  let default_executable_name  = c "default_executable_name"
  let systhread_supported      = c "systhread_supported"
  
  
  (** {2 Paths} *)
  
  (**/**)
  let p name hlp dflt = 
    var_define
      ~short_desc:hlp 
      ~cli:CLIAuto 
      ~arg_help:"dir" 
      name 
      dflt 
  
  let (/) a b = 
    if os_type () = Sys.os_type then
      Filename.concat a b 
    else if os_type () = "Unix" then
      BaseFilePath.Unix.concat a b 
    else
      OASISUtils.failwithf1 
        (f_ "Cannot handle os_type %s filename concat")
        (os_type ())
  (**/**)
  
  let prefix = 
    p "prefix"
      (fun () -> s_ "Install architecture-independent files dir")
      (lazy 
         (match os_type () with
            | "Win32" ->
                let program_files =
                  Sys.getenv "PROGRAMFILES"
                in
                  program_files/(pkg_name ())
            | _ ->
                "/usr/local"))
  
  let exec_prefix = 
    p "exec_prefix"
      (fun () -> s_ "Install architecture-dependent files in dir")
      (lazy "$prefix")
  
  let bindir =
    p "bindir"
      (fun () -> s_ "User executables")
      (lazy ("$exec_prefix"/"bin"))
  
  let sbindir =
    p "sbindir"
      (fun () -> s_ "System admin executables")
      (lazy ("$exec_prefix"/"sbin"))
  
  let libexecdir =
    p "libexecdir"
      (fun () -> s_ "Program executables")
      (lazy ("$exec_prefix"/"libexec"))
  
  let sysconfdir =
    p "sysconfdir"
      (fun () -> s_ "Read-only single-machine data")
      (lazy ("$prefix"/"etc"))
  
  let sharedstatedir =
    p "sharedstatedir"
      (fun () -> s_ "Modifiable architecture-independent data")
      (lazy ("$prefix"/"com"))
  
  let localstatedir =
    p "localstatedir"
      (fun () -> s_ "Modifiable single-machine data")
      (lazy ("$prefix"/"var"))
  
  let libdir =
    p "libdir"
      (fun () -> s_ "Object code libraries")
      (lazy ("$exec_prefix"/"lib"))
  
  let datarootdir =
    p "datarootdir"
      (fun () -> s_ "Read-only arch-independent data root")
      (lazy ("$prefix"/"share"))
  
  let datadir =
    p "datadir"
      (fun () -> s_ "Read-only architecture-independent data")
      (lazy ("$datarootdir"))
  
  let infodir =
    p "infodir"
      (fun () -> s_ "Info documentation")
      (lazy ("$datarootdir"/"info"))
  
  let localedir =
    p "localedir"
      (fun () -> s_ "Locale-dependent data")
      (lazy ("$datarootdir"/"locale"))
  
  let mandir =
    p "mandir"
      (fun () -> s_ "Man documentation")
      (lazy ("$datarootdir"/"man"))
  
  let docdir =
    p "docdir"
      (fun () -> s_ "Documentation root")
      (lazy ("$datarootdir"/"doc"/"$pkg_name"))
  
  let htmldir =
    p "htmldir"
      (fun () -> s_ "HTML documentation")
      (lazy ("$docdir"))
  
  let dvidir =
    p "dvidir"
      (fun () -> s_ "DVI documentation")
      (lazy ("$docdir"))
  
  let pdfdir =
    p "pdfdir"
      (fun () -> s_ "PDF documentation")
      (lazy ("$docdir"))
  
  let psdir =
    p "psdir"
      (fun () -> s_ "PS documentation")
      (lazy ("$docdir"))
  
  let destdir =
    p "destdir"
      (fun () -> s_ "Prepend a path when installing package")
      (lazy 
         (raise 
            (PropList.Not_set
               ("destdir", 
                Some (s_ "undefined by construct")))))
  
  (** {2 ...} *)
  
  (** Findlib version
    *)
  let findlib_version =
    var_define
      "findlib_version"
      (lazy 
         (BaseCheck.package_version "findlib"))
  
  (** Check that the platform is a native platform (can compile native
      exec/library).
    *)
  let is_native =
    var_define
      "is_native"
      (lazy
         (try
            let _s : string = 
              ocamlopt ()
            in
              "true"
          with PropList.Not_set _ ->
            let _s : string = 
              ocamlc ()
            in
              "false"))
  
  (** Compute the default suffix for program (target OS dependent)
    *)
  let suffix_program =
    var_define
      "suffix_program"
      (lazy
         (match os_type () with 
            | "Win32" -> ".exe" 
            | _ -> ""
         ))
  
  (** Initialize some variables 
    *)
  let init pkg = 
    rpkg := Some pkg
  
end

module BaseFileAB = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseFileAB.ml"
  
  (** File .ab which content will be replaced by environment variable
     
      This is the same kind of file as .in file for autoconf, except we
      use the variable definition of ${!Buffer.add_substitute}. This is 
      the default file to be generated by configure step (even for 
      autoconf, except that it produce a master file before).
  
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISGettext
  
  let to_filename fn =
    let fn =
      BaseFilePath.of_unix fn
    in
      if not (Filename.check_suffix fn ".ab") then
        OASISMessage.warning 
          (f_ "File '%s' doesn't have '.ab' extension")
          fn;
      Filename.chop_extension fn
  
  (** Replace variable in file %.ab to generate %
    *)
  let replace fn_lst =
    let buff =
      Buffer.create 13
    in
      List.iter
        (fun fn ->
           let fn =
             BaseFilePath.of_unix fn
           in
           let chn_in =
             open_in fn
           in
           let chn_out =
             open_out (to_filename fn)
           in
             (
               try
                 while true do
                  Buffer.add_string buff (var_expand (input_line chn_in));
                  Buffer.add_char buff '\n'
                 done
               with End_of_file ->
                 ()
             );
             Buffer.output_buffer chn_out buff;
             Buffer.clear buff;
             close_in chn_in;
             close_out chn_out)
        fn_lst
end

module BaseLog = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseLog.ml"
  
  (** Maintain a DB of what has been done
      @author Sylvain Le Gall
    *)
  
  open OASISUtils
  
  (** Default file for registering log
    *)
  let default_filename =
    Filename.concat 
      (Filename.dirname BaseEnv.default_filename)
      "setup.log"
  
  module SetTupleString =
    Set.Make
      (struct 
         type t = string * string
         let compare (s11, s12) (s21, s22) = 
           match String.compare s11 s21 with 
             | 0 -> String.compare s12 s22
             | n -> n
       end)
  
  (** Load the log file
    *)
  let load () = 
    if Sys.file_exists default_filename then
      (
        let chn = 
          open_in default_filename
        in
        let rec read_aux (st, acc) =
          try 
            let line = 
              input_line chn
            in
            let t = 
              Scanf.sscanf line "%S %S" 
                (fun e d ->  e, d)
            in
              read_aux 
                (if SetTupleString.mem t st then
                   st, acc
                 else
                   SetTupleString.add t st,
                   t :: acc)
          with End_of_file ->
            close_in chn;
            List.rev acc
        in
          read_aux (SetTupleString.empty, [])
      )
    else
      (
        []
      )
  
  (** Add an event to the log file
    *)
  let register event data =
    let chn_out =
      open_out_gen [Open_append; Open_creat; Open_text] 0o644 default_filename
    in
      Printf.fprintf chn_out "%S %S\n" event data;
      close_out chn_out
  
  (** Remove an event from the log file
    *)
  let unregister event data =
    if Sys.file_exists default_filename then
      begin
        let lst = 
          load ()
        in
        let chn_out =
          open_out default_filename
        in
        let write_something =
          ref false
        in
          List.iter 
            (fun (e, d) ->
               if e <> event || d <> data then
                 begin
                   write_something := true;
                   Printf.fprintf chn_out "%S %S\n" e d
                 end)
            lst;
          close_out chn_out;
          if not !write_something then
            Sys.remove default_filename
      end
  
  (** Filter events of the log file
    *)
  let filter events =
    let st_events =
      List.fold_left
        (fun st e -> 
           SetString.add e st)
        SetString.empty
        events
    in
      List.filter 
        (fun (e, _) -> SetString.mem e st_events)
        (load ())
  
  (** Check if an event exists in the log file 
    *)
  let exists event data =
    List.exists
      (fun v -> (event, data) = v)
      (load ())
end

module BaseBuilt = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseBuilt.ml"
  
  (** Register files built to be installed
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open OASISGettext
  open BaseStandardVar
  
  type filenames = filename list
  
  type t =
    | BExec    (* Executable *)
    | BExecLib (* Library coming with executable *)
    | BLib     (* Library *)
    | BDoc     (* Documentation *)
  
  let to_log_event t nm = 
    "built_"^
    (match t with 
       | BExec -> "exec"
       | BExecLib -> "exec_lib"
       | BLib -> "lib"
       | BDoc -> "doc")^
    "_"^nm
  
  (* Register files built *)
  let register t nm lst = 
    List.iter
      (fun fn ->
         BaseLog.register 
           (to_log_event t nm)
           fn)
      lst
  
  (* Unregister all files built *)
  let unregister t nm =
    List.iter
      (fun (e, d) ->
         BaseLog.unregister e d)
      (BaseLog.filter [to_log_event t nm])
  
  (* Fold-left files built, filter existing
     and non-existing files.
   *)
  let fold t nm f acc = 
    List.fold_left 
      (fun acc (_, fn) ->
         if Sys.file_exists fn then
           begin
             f acc fn
           end
         else
           begin
             OASISMessage.warning 
               (f_ "File '%s' has been marked as built \
                  for %s but doesn't exist")
               fn
               (Printf.sprintf
                  (match t with 
                     | BExec | BExecLib -> 
                         (f_ "executable %s")
                     | BLib ->
                         (f_ "library %s")
                     | BDoc ->
                         (f_ "documentation %s"))
                  nm);
             acc
           end)
      acc
      (BaseLog.filter
         [to_log_event t nm])
  
  (** Generate the list of files that should be built for 
      an executable.
    *)
  let of_executable ffn (cs, bs, exec) = 
    let unix_exec_is, unix_dll_opt = 
      OASISExecutable.unix_exec_is 
        (cs, bs, exec)
        (fun () -> 
           bool_of_string 
             (is_native ()))
        ext_dll
        suffix_program
    in
    let evs = 
      (BExec, cs.cs_name, [ffn unix_exec_is])
      ::
      (match unix_dll_opt with
         | Some fn ->
             [BExecLib, cs.cs_name, [ffn fn]]
         | None ->
             [])
    in
      evs,
      unix_exec_is,
      unix_dll_opt
  
  (** Generate the list of files that should be built for
      a library
    *)
  let of_library ffn (cs, bs, lib) = 
    let unix_lst = 
      OASISLibrary.generated_unix_files
        (cs, bs, lib)
        (fun fn ->
           Sys.file_exists (BaseFilePath.of_unix fn))
        (fun () ->
           bool_of_string (is_native ()))
        ext_lib
        ext_dll
    in
    let evs =
      [BLib,
       cs.cs_name,
       List.map ffn unix_lst]
    in
      evs, unix_lst
  
  (* Unregister all build event related to a package
   *)
  let clean_all pkg =
      List.fold_left
        (fun () ->
           function
             | Library (cs, _, _) -> 
                 unregister BLib cs.cs_name
             | Executable (cs, _, _) ->
                 unregister BExec cs.cs_name;
                 unregister BExecLib cs.cs_name
             | Flag _ | SrcRepo _ | Doc _ | Test _ ->
                 ())
        ()
        pkg.sections
end

module BaseCustom = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseCustom.ml"
  
  (** Run custom command for pre/post hook
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISTypes
  open OASISGettext
  
  (* Expand and run command *)
  let run cmd args extra_args =
    BaseExec.run 
      (var_expand cmd)
      (List.map 
         var_expand
         (args @ (Array.to_list extra_args)))
  
  (* Apply a function nested in a custom block
   *)
  let hook ?(failsafe=false) cstm f e =
    let optional_command lst = 
      let printer =
        function 
          | Some (cmd, args) -> String.concat " " (cmd :: args)
          | None -> s_ "No command"
      in
        match var_choose ~printer lst with 
          | Some (cmd, args) ->
              begin
                try 
                  run cmd args [||]
                with e when failsafe ->
                  OASISMessage.warning 
                    (f_ "Command '%s' fail with error: %s")
                    (String.concat " " (cmd :: args))
                    (match e with 
                       | Failure msg -> msg
                       | e -> Printexc.to_string e)
              end
          | None ->
              ()
    in
    let res =
      optional_command cstm.pre_command;
      f e
    in
      optional_command cstm.post_command;
      res
end

module BaseTest = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseTest.ml"
  
  (** Run test
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISMessage
  open OASISTypes
  open OASISExpr
  open OASISGettext
  
  let test lst pkg extra_args =
  
    let one_test (test_plugin, cs, test) =
      if var_choose test.test_run then
        begin
          let () = 
            info (f_ "Running test '%s'") cs.cs_name
          in
          let back_cwd = 
            match test.test_working_directory with 
              | Some dir -> 
                  let cwd = 
                    Sys.getcwd ()
                  in
                  let chdir d =
                    info (f_ "Changing directory to '%s'") d;
                    Sys.chdir d
                  in
                    chdir dir;
                    fun () -> chdir cwd
  
              | None -> 
                  fun () -> ()
          in
            try 
              let failure_percent =
                BaseCustom.hook 
                  test.test_custom
                  (test_plugin pkg (cs, test))
                  extra_args 
              in
                back_cwd ();
                failure_percent
            with e ->
              begin
                back_cwd ();
                raise e
              end
        end
      else
        begin
          info (f_ "Skipping test '%s'") cs.cs_name;
          0.0
        end
    in
    let res =
      List.map
        one_test
        lst
    in
    let n = 
      float_of_int (List.length res)
    in
    let failure_percent =
      List.fold_left
        (fun r e -> r +. (e /. n))
        0.0
        res
    in
      (if failure_percent > 0.0 then
         warning 
       else
         info)
        (f_ "Tests had a %.2f%% failure rate")
        (100. *. failure_percent)
  
end

module BaseDoc = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseDoc.ml"
  
  (** Build documentation
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISTypes
  open OASISGettext
  
  let doc lst pkg extra_args =
  
    let one_doc (doc_plugin, cs, doc) = 
      if var_choose doc.doc_build then
        begin
          OASISMessage.info (f_ "Building documentation '%s'") cs.cs_name;
          BaseCustom.hook
            doc.doc_custom
            (doc_plugin pkg (cs, doc))
            extra_args
        end
    in
      List.iter 
        one_doc
        lst
  
end

module BaseSetup = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseSetup.ml"
  
  (** Entry points for setup.ml
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open OASISMessage
  open OASISTypes
  open OASISSection
  open OASISGettext
  open OASISUtils
  
  type std_args_fun = 
      package -> string array -> unit
  
  type ('a, 'b) section_args_fun = 
      name * (package -> (common_section * 'a) -> string array -> 'b)
  
  type t =
      {
        configure:       std_args_fun;
        build:           std_args_fun;
        doc:             ((doc, unit)  section_args_fun) list;
        test:            ((test, float) section_args_fun) list;
        install:         std_args_fun;
        uninstall:       std_args_fun;
        clean:           std_args_fun list;
        clean_doc:       (doc, unit) section_args_fun list;
        clean_test:      (test, unit) section_args_fun list;
        distclean:       std_args_fun list;
        distclean_doc:   (doc, unit) section_args_fun list;
        distclean_test:  (test, unit) section_args_fun list;
        package:         package;
      } 
  
  (** Associate a plugin function with data from package
    *)
  let join_plugin_sections filter_map lst =
    List.rev
      (List.fold_left
         (fun acc sct ->
            try 
              match filter_map sct with 
                | Some e ->
                    e :: acc
                | None ->
                    acc
            with Not_found ->
              failwithf1
                (f_ "Cannot find plugin matching %s")
                (OASISSection.string_of_section sct))
         []
         lst)
  
  (** Configure step *)
  let configure t args = 
    (* Run configure *)
    BaseCustom.hook 
      t.package.conf_custom
      (t.configure t.package)
      args;
  
    (* Reload environment *)
    unload ();
    load ();
  
    (* Replace data in file *)
    BaseFileAB.replace t.package.files_ab
  
  (** Build step *)
  let build t args =
    BaseCustom.hook
      t.package.build_custom
      (t.build t.package)
      args
  
  (** Documentation step *)
  let doc t args =
    BaseDoc.doc
      (join_plugin_sections
         (function 
            | Doc (cs, e) -> 
                Some 
                  (List.assoc cs.cs_name t.doc,
                   cs,
                   e)
            | _ -> 
                None)
         t.package.sections)
      t.package
      args
  
  (** Test step *)
  let test t args = 
    BaseTest.test 
      (join_plugin_sections
         (function 
            | Test (cs, e) -> 
                Some 
                  (List.assoc cs.cs_name t.test,
                   cs,
                   e)
            | _ -> 
                None)
         t.package.sections)
      t.package
      args
  
  (** Install step *)
  let install t args =
    BaseCustom.hook
      t.package.install_custom
      (t.install t.package)
      args
  
  (** Uninstall step *)
  let uninstall t args =
    BaseCustom.hook
      t.package.uninstall_custom
      (t.uninstall t.package)
      args
  
  (** Clean and distclean steps *)
  let clean, distclean = 
    let generic_clean t what cstm mains docs tests args = 
      BaseCustom.hook
        ~failsafe:true
        cstm
        (fun () ->
           (* Clean section *)
           List.iter
             (function
                | Test (cs, test) ->
                    let f =
                      List.assoc cs.cs_name tests
                    in
                      f t.package (cs, test) args
                | Library _ 
                | Executable _
                | Flag _ 
                | SrcRepo _
                | Doc _ ->
                    ())
             t.package.sections;
           (* Clean whole package *)
           List.iter
             (fun f -> 
                f t.package args)
             mains)
        ()
    in
  
    let clean t args =
      generic_clean 
        t 
        "cleaning" 
        t.package.clean_custom
        t.clean 
        t.clean_doc 
        t.clean_test 
        args
    in
  
    let distclean t args =
      (* Call clean *)
      clean t args;
  
      (* Remove generated file *)
      List.iter
        (fun fn ->
           if Sys.file_exists fn then
             begin
               info (f_ "Remove '%s'") fn;
               Sys.remove fn
             end)
        (BaseEnv.default_filename 
         :: 
         BaseLog.default_filename
         ::
         (List.rev_map BaseFileAB.to_filename t.package.files_ab));
      
      (* Call distclean code *)
      generic_clean 
        t 
        "distcleaning" 
        t.package.distclean_custom
        t.distclean 
        t.distclean_doc 
        t.distclean_test 
        args
    in
  
      clean, distclean
  
  let setup t = 
    let catch_exn =
      ref true
    in
      try
        let act_ref =
          ref (fun _ -> 
                 failwithf2
                   (f_ "No action defined, run '%s %s -help'")
                   Sys.executable_name
                   Sys.argv.(0))
  
        in
        let extra_args_ref =
          ref []
        in
        let allow_empty_env_ref = 
          ref false
        in
        let arg_handle ?(allow_empty_env=false) act =
          Arg.Tuple
            [
              Arg.Rest (fun str -> extra_args_ref := str :: !extra_args_ref);
  
              Arg.Unit 
                (fun () -> 
                   allow_empty_env_ref := allow_empty_env;
                   act_ref := act);
            ]
        in
  
          Arg.parse 
            (Arg.align
               [
                 "-configure",
                 arg_handle ~allow_empty_env:true configure,
                 s_ "[options*] Configure build process.";
  
                 "-build",
                 arg_handle build,
                 s_ "[options*] Run build process.";
  
                 "-doc",
                 arg_handle doc,
                 s_ "[options*] Build documentation.";
  
                 "-test",
                 arg_handle test,
                 s_ "[options*] Run tests.";
  
                 "-install",
                 arg_handle install,
                 s_ "[options*] Install libraries, data, executables \
                                and documentations.";
  
                 "-uninstall",
                 arg_handle uninstall,
                 s_ "[options*] Uninstall libraries, data, executables \
                                and documentations.";
  
                 "-clean",
                 arg_handle ~allow_empty_env:true clean,
                 s_ "[options*] Clean build environment.";
  
                 "-distclean",
                 arg_handle ~allow_empty_env:true distclean,
                 s_ "[options*] Clean build and configure environment.";
  
                 "-no-catch-exn",
                 Arg.Clear catch_exn,
                 s_ " Don't catch exception, useful for debugging.";
               ] 
             @ (OASISMessage.args ()))
            (failwithf1 (f_ "Don't know what to do with '%s'"))
            (s_ "Setup and run build process current package\n");
  
          (* Build initial environment *)
          load ~allow_empty:!allow_empty_env_ref ();
  
          (** Initialize flags *)
          List.iter
            (function
               | Flag (cs, {flag_description = hlp; 
                            flag_default = choices}) ->
                   begin
                     let apply ?short_desc () = 
                       var_ignore
                         (var_define
                            ~cli:CLIEnable
                            ?short_desc
                            cs.cs_name
                            (lazy (string_of_bool 
                                     (var_choose choices))))
                     in
                       match hlp with 
                         | Some hlp ->
                             apply ~short_desc:(fun () -> hlp) ()
                         | None ->
                             apply ()
                   end
               | _ -> 
                   ())
            t.package.sections;
  
          BaseStandardVar.init (t.package.name, t.package.version);
  
          !act_ref t (Array.of_list (List.rev !extra_args_ref))
  
      with e when !catch_exn ->
        begin
          try 
            error "%s" (PropList.string_of_exception e)
          with 
            | Failure s ->
                error "%s" s
            | e ->
                error "%s" (Printexc.to_string e)
        end
  
end

module BaseDev = struct
# 0 "/home/gildor/programmation/oasis/src/base/BaseDev.ml"
  
  (** Entry points for setup.ml in dev mode
    *)
  
  
  
  open OASISGettext
  
  type t = 
      {
        oasis_cmd:  string;
        oasis_args: string list;
        self_fn:    string;
      } 
  
  let update_and_run t = 
    let dev_fn = 
      "setup-dev.ml"
    in
  
    (* Command to run after creation of dev_fn *)
    let bootstrap_ocaml = 
      Sys.executable_name
    in
    let bootstrap_args =
      Array.to_list
        (Array.map
           (fun a ->
              if a = t.self_fn then
                dev_fn
              else
                a)
           Sys.argv)
    in
  
    let safe_exit () = 
      if Sys.file_exists dev_fn then
        Sys.remove dev_fn
    in
  
      if Sys.file_exists dev_fn then
        OASISMessage.error
          (f_ "File %s already exists, cannot generate it for \
               dev-mode. Please remove it first.")
          dev_fn;
  
      try 
        (* Run OASIS to generate a temporary setup.ml
         *)
        BaseExec.run 
          t.oasis_cmd 
          ("-quiet" :: "-setup-fn" :: dev_fn :: t.oasis_args);
        (* Run own command line by replacing setup.ml by 
         * setup-dev.ml
         *)
        BaseExec.run
          bootstrap_ocaml
          bootstrap_args;
  
        (* Clean dev file *)
        safe_exit ()
  
      with e ->
        safe_exit ();
        raise e
  
end


# 3881 "setup.ml"
module InternalConfigurePlugin = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/internal/InternalConfigurePlugin.ml"
  
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
  
end

module InternalInstallPlugin = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/internal/InternalInstallPlugin.ml"
  
  (** Install using internal scheme
      @author Sylvain Le Gall
    *)
  
  open BaseEnv
  open BaseStandardVar
  open OASISMessage
  open OASISTypes
  open OASISLibrary
  open OASISGettext
  open OASISUtils
  
  let exec_hook =
    ref (fun (cs, bs, exec) -> cs, bs, exec)
  
  let lib_hook =
    ref (fun (cs, bs, lib) -> cs, bs, lib, [])
  
  let doc_hook =
    ref (fun (cs, doc) -> cs, doc)
  
  let install_file_ev = 
    "install-file"
  
  let install_dir_ev =
    "install-dir"
  
  let install_findlib_ev =
    "install-findlib"
  
  let install pkg argv =
  
    let in_destdir =
      try 
        let destdir =
          destdir () 
        in
          (* Practically speaking destdir is prepended
           * at the beginning of the target filename
           *)
          fun fn -> destdir^fn
      with PropList.Not_set _ ->
        fun fn -> fn
    in
  
    let install_file src_file envdir = 
      let tgt_dir = 
        in_destdir (envdir ())
      in
      let tgt_file =
        Filename.concat 
          tgt_dir
          (Filename.basename src_file)
      in
        (* Check that target directory exist *)
        if not (Sys.file_exists tgt_dir) then
          (
            info (f_ "Creating directory '%s'") tgt_dir;
            BaseFileUtil.mkdir tgt_dir;
            BaseLog.register install_dir_ev tgt_dir
          );
  
        (* Really install files *)
        info (f_ "Copying file '%s' to '%s'") src_file tgt_file;
        BaseFileUtil.cp src_file tgt_file;
        BaseLog.register install_file_ev tgt_file
    in
  
    (* Install all datas *)
    let install_datas pkg = 
  
      (* Install data into defined directory *)
      let install_data srcdir lst tgtdir =
        let tgtdir = 
          var_expand tgtdir
        in
          List.iter
            (fun (src, tgt_opt) ->
               let real_srcs = 
                 BaseFileUtil.glob 
                   (Filename.concat srcdir src)
               in
                 if real_srcs = [] then
                   failwithf1
                     (f_ "Wildcard '%s' doesn't match any files")
                     src;
                 List.iter 
                   (fun fn -> 
                      install_file 
                        fn 
                        (fun () -> 
                           match tgt_opt with 
                             | Some s -> var_expand s
                             | None -> tgtdir))
                   real_srcs)
            lst
      in       
  
        List.iter
          (function
             | Library (_, bs, _)
             | Executable (_, bs, _) ->
                 install_data
                   bs.bs_path
                   bs.bs_data_files
                   (Filename.concat 
                      (datarootdir ())
                      pkg.name)
             | Doc (_, doc) ->
                 install_data
                   Filename.current_dir_name
                   doc.doc_data_files
                   doc.doc_install_dir
             | _ ->
                 ())
          pkg.sections
    in
  
    (** Install all libraries *)
    let install_libs pkg =
  
      let files_of_library acc data_lib = 
        let cs, bs, lib, lib_extra =
          !lib_hook data_lib
        in
          if var_choose bs.bs_install then
            begin
              let acc = 
                (* Start with acc + lib_extra *)
                List.rev_append lib_extra acc
              in
              let acc = 
                (* Add uncompiled header from the source tree *)
                let path = 
                  BaseFilePath.of_unix bs.bs_path
                in
                  List.fold_left
                    (fun acc modul ->
                       try 
                         List.find
                           Sys.file_exists 
                           (List.map
                              (Filename.concat path)
                              [modul^".mli";
                               modul^".ml";
                               String.uncapitalize modul^".mli";
                               String.capitalize   modul^".mli";
                               String.uncapitalize modul^".ml";
                               String.capitalize   modul^".ml"])
                         :: acc
                       with Not_found ->
                         begin
                           warning 
                             (f_ "Cannot find source header for module %s \
                                  in library %s")
                             modul cs.cs_name;
                           acc
                         end)
                    acc
                    lib.lib_modules
              in
               (* Get generated files *)
               BaseBuilt.fold 
                 BaseBuilt.BLib 
                 cs.cs_name
                 (fun acc fn -> fn :: acc)
                 acc
            end
           else
            begin
              acc
            end
      in
  
      (* Install one group of library *)
      let install_group_lib grp = 
        (* Iterate through all group nodes *)
        let rec install_group_lib_aux acc grp =
          let acc, children = 
            match grp with 
              | Container (_, children) ->
                  acc, children
              | Package (_, cs, bs, lib, children) ->
                  files_of_library acc (cs, bs, lib), children
          in
            List.fold_left
              install_group_lib_aux
              acc
              children
        in
  
        (* Findlib name of the root library *)
        let findlib_name =
          findlib_of_group grp
        in
  
        (* Determine root library *)
        let root_lib =
          root_of_group grp
        in
  
        (* All files to install for this library *)
        let files =
          install_group_lib_aux [] grp
        in
  
          (* Really install, if there is something to install *)
          if files = [] then 
            begin
              warning
                (f_ "Nothing to install for findlib library '%s'")
                findlib_name
            end
          else
            begin
              let meta = 
                (* Search META file *)
                let (_, bs, _) = 
                  root_lib
                in
                let res = 
                  Filename.concat bs.bs_path "META"
                in
                  if not (Sys.file_exists res) then
                    failwithf2
                      (f_ "Cannot find file '%s' for findlib library %s")
                      res
                      findlib_name;
                  res
              in
                info 
                  (f_ "Installing findlib library '%s'")
                  findlib_name;
                BaseExec.run 
                  (ocamlfind ()) 
                  ("install" :: findlib_name :: meta :: files);
                BaseLog.register install_findlib_ev findlib_name 
            end
      in
  
        (* We install libraries in groups *)
        List.iter 
          install_group_lib
          (group_libs pkg)
    in
  
    let install_execs pkg = 
      let install_exec data_exec =
        let (cs, bs, exec) =
          !exec_hook data_exec
        in
          if var_choose bs.bs_install then
            begin
              let exec_libdir () =
                Filename.concat 
                  (libdir ())
                  pkg.name
              in
                BaseBuilt.fold
                  BaseBuilt.BExec
                  cs.cs_name
                  (fun () fn ->
                     install_file
                       fn
                       bindir)
                  ();
                BaseBuilt.fold
                  BaseBuilt.BExecLib
                  cs.cs_name
                  (fun () fn ->
                     install_file
                       fn
                       exec_libdir)
                  ()
            end
      in
        List.iter
          (function
             | Executable (cs, bs, exec)->
                 install_exec (cs, bs, exec)
             | _ ->
                 ())
          pkg.sections
    in
  
    let install_docs pkg = 
      let install_doc data =
        let (cs, doc) =
          !doc_hook data
        in
          if var_choose doc.doc_install then
            begin
              let tgt_dir =
                var_expand doc.doc_install_dir
              in
                BaseBuilt.fold
                  BaseBuilt.BDoc
                  cs.cs_name
                  (fun () fn ->
                     install_file 
                       fn 
                       (fun () -> tgt_dir))
                ()
            end
      in
        List.iter
          (function
             | Doc (cs, doc) ->
                 install_doc (cs, doc)
             | _ ->
                 ())
          pkg.sections
    in
    
      install_libs  pkg;
      install_execs pkg;
      install_docs  pkg;
      install_datas pkg
  
  (* Uninstall already installed data *)
  let uninstall _ argv =
    List.iter 
      (fun (ev, data) ->
         if ev = install_file_ev then
           begin
             if Sys.file_exists data then
               begin
                 info
                   (f_ "Removing file '%s'")
                   data;
                 Sys.remove data
               end
           end 
         else if ev = install_dir_ev then
           begin
             if Sys.file_exists data && Sys.is_directory data then
               begin
                 if Sys.readdir data = [||] then
                   begin
                     info
                       (f_ "Removing directory '%s'")
                       data;
                     BaseFileUtil.rmdir data
                   end
                 else
                   begin
                     warning 
                       (f_ "Directory '%s' is not empty (%s)")
                       data
                       (String.concat 
                          ", " 
                          (Array.to_list 
                             (Sys.readdir data)))
                   end
               end
           end
         else if ev = install_findlib_ev then
           begin
             info (f_ "Removing findlib library '%s'") data;
             BaseExec.run (ocamlfind ()) ["remove"; data]
           end
         else
           failwithf1 (f_ "Unknown log event '%s'") ev;
         BaseLog.unregister ev data)
      (* We process event in reverse order *)
      (List.rev 
         (BaseLog.filter 
            [install_file_ev; 
             install_dir_ev;
             install_findlib_ev;]))
  
end


# 4388 "setup.ml"
module OCamlbuildCommon = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/ocamlbuild/OCamlbuildCommon.ml"
  
  (** Functions common to OCamlbuild build and doc plugin
    *)
  
  open BaseEnv
  open BaseStandardVar
  
  let ocamlbuild_clean_ev =
    "ocamlbuild-clean"
  
  (** Fix special arguments depending on environment *)
  let fix_args args extra_argv =
    List.flatten
      [
        if (os_type ()) = "Win32" then
          [
            "-classic-display"; 
            "-no-log"; 
            "-no-links";
            "-install-lib-dir"; 
            (Filename.concat (standard_library ()) "ocamlbuild")
          ] 
        else
          [];
    
        if not (bool_of_string (is_native ())) || (os_type ()) = "Win32" then
          [
            "-byte-plugin" 
          ]
        else
          [];
        args;
        Array.to_list extra_argv;
      ]
  
  (** Run 'ocamlbuild -clean' if not already done *)
  let run_clean extra_argv =
    let extra_cli =
      String.concat " " (Array.to_list extra_argv)
    in
      (* Run if never called with these args *)
      if not (BaseLog.exists ocamlbuild_clean_ev extra_cli) then
        begin
          BaseExec.run (ocamlbuild ()) (fix_args ["-clean"] extra_argv);
          BaseLog.register ocamlbuild_clean_ev extra_cli;
          at_exit 
            (fun () ->
               try 
                 BaseLog.unregister ocamlbuild_clean_ev extra_cli
               with _ ->
                 ())
        end
  
  (** Run ocamlbuild, unregister all clean events *)
  let run_ocamlbuild args extra_argv =
    BaseExec.run (ocamlbuild ()) (fix_args args extra_argv);
    (* Remove any clean event, we must run it again *)
    List.iter
      (fun (e, d) -> BaseLog.unregister e d)
      (BaseLog.filter [ocamlbuild_clean_ev])
  
  (** Determine real build directory *)
  let build_dir extra_argv =
    let rec search_args dir =
      function
        | "-build-dir" :: dir :: tl ->
            search_args dir tl
        | _ :: tl ->
            search_args dir tl
        | [] -> 
            dir
    in
      search_args "_build" (fix_args [] extra_argv)
end

module OCamlbuildPlugin = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/ocamlbuild/OCamlbuildPlugin.ml"
  
  (** Build using ocamlbuild  
      @author Sylvain Le Gall
    *)
  
  open OASISTypes
  open OASISGettext
  open OASISUtils
  open BaseEnv
  open BaseStandardVar
  
  type target =
    | Std of string 
    | StdRename of string * string
  
  let cond_targets_hook =
    ref (fun lst -> lst)
  
  let build pkg argv =
  
    (* Return the filename in build directory *)
    let in_build_dir fn =
      Filename.concat 
        (OCamlbuildCommon.build_dir argv) 
        fn
    in
  
    (* Return the unix filename in host build directory *)
    let in_build_dir_of_unix fn =
      in_build_dir (BaseFilePath.of_unix fn)
    in
  
    let cond_targets =
      List.fold_left
        (fun acc ->
           function 
             | Library (cs, bs, lib) when var_choose bs.bs_build ->
                 begin
                   let evs, unix_files =
                     BaseBuilt.of_library 
                       in_build_dir_of_unix
                       (cs, bs, lib)
                   in
  
                   let ends_with nd fn =
                     let nd_len =
                       String.length nd
                     in
                       (String.length fn >= nd_len)
                       &&
                       (String.sub 
                          fn
                          (String.length fn - nd_len)
                          nd_len) = nd
                   in
  
                   let tgts =
                     List.filter
                       (fun fn ->
                          ends_with ".cma" fn ||
                          ends_with ".cmxa" fn ||
                          ends_with (ext_lib ()) fn ||
                          ends_with (ext_dll ()) fn)
                       unix_files
                   in
  
                     match tgts with 
                       | hd :: tl ->
                           (evs, Std hd)
                           :: 
                           (List.map (fun tgt -> [], Std tgt) tl)
                           @
                           acc
                       | [] ->
                           failwithf2
                             (f_ "No possible ocamlbuild targets \
                                  in generated files %s for library %s")
                             (String.concat (s_ ", " ) tgts)
                             cs.cs_name
                 end
  
             | Executable (cs, bs, exec) when var_choose bs.bs_build ->
                 begin
                   let evs, unix_exec_is, unix_dll_opt =
                     BaseBuilt.of_executable 
                       in_build_dir_of_unix
                       (cs, bs, exec)
                   in
  
                   let host_exec_is = 
                     in_build_dir_of_unix unix_exec_is
                   in
  
                   let target ext =
                     let unix_tgt = 
                       (BaseFilePath.Unix.concat
                          bs.bs_path
                          (BaseFilePath.Unix.chop_extension 
                             exec.exec_main_is))^ext
                     in
  
                       evs,
                       (if unix_tgt = unix_exec_is then
                          Std unix_tgt
                        else
                          StdRename (unix_tgt, host_exec_is))
                   in
  
                   (* Add executable *)
                   let acc =
                     match bs.bs_compiled_object with
                       | Native ->
                           (target ".native") :: acc
                       | Best when bool_of_string (is_native ()) ->
                           (target ".native") :: acc
                       | Byte
                       | Best ->
                           (target ".byte") :: acc
                   in
                     acc
                 end
  
             | Library _ | Executable _ | Test _ 
             | SrcRepo _ | Flag _ | Doc _ ->
                 acc)
        []
        (* Keep the pkg.sections ordered *)
        (List.rev pkg.sections);
    in
  
    (* Check and register built files *)
    let check_and_register (bt, bnm, lst) = 
      List.iter
        (fun fn ->
           if not (Sys.file_exists fn) then
             failwithf1
               (f_ "Expected built file '%s' doesn't exist")
               fn)
        lst;
        (BaseBuilt.register bt bnm lst) 
    in
  
    (* Run a list of target + post process *)
    let run_ocamlbuild rtargets = 
      OCamlbuildCommon.run_ocamlbuild 
        (List.rev_map snd rtargets)
        argv;
      List.iter
        check_and_register
        (List.flatten (List.rev_map fst rtargets))
    in
  
    let last_rtargets =
      List.fold_left
        (fun acc (built, tgt) ->
           match tgt with 
             | Std nm -> 
                 (built, nm) :: acc
             | StdRename (src, tgt) ->
                 (* We run with a fake list for event registering *)
                 run_ocamlbuild (([], src) :: acc);
                 (* And then copy and register *)
                 BaseFileUtil.cp 
                   (in_build_dir_of_unix src)
                   tgt;
                 List.iter check_and_register built;
                 [])
        []
        (!cond_targets_hook cond_targets)
    in
      if last_rtargets <> [] then
        run_ocamlbuild last_rtargets
  
  let clean pkg extra_args  = 
    OCamlbuildCommon.run_clean extra_args;
    BaseBuilt.clean_all pkg
  
end

module OCamlbuildDocPlugin = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/ocamlbuild/OCamlbuildDocPlugin.ml"
  
  (* Create documentation using ocamlbuild .odocl files
     @author Sylvain Le Gall
   *)
  
  open OASISTypes
  open OASISGettext
  open OASISMessage
  open BaseStandardVar
  
  
  
  type t =
      {
        path:    dirname;
        modules: string list;
      } 
  
  let doc_build t pkg (cs, doc) argv =
    let index_html =
      t.path^"/"^cs.cs_name^".docdir/index.html"
    in
    let tgt_dir =
      BaseFilePath.make
        [
          OCamlbuildCommon.build_dir argv;
          BaseFilePath.of_unix t.path;
          cs.cs_name^".docdir";
        ]
    in
      OCamlbuildCommon.run_ocamlbuild [index_html] argv;
      List.iter
        (fun glb ->
           BaseBuilt.register
             BaseBuilt.BDoc
             cs.cs_name
             (BaseFileUtil.glob 
                (Filename.concat tgt_dir glb)))
        ["*.html"; "*.css"]
  
  let doc_clean t pkg (cs, doc) argv =
    OCamlbuildCommon.run_clean argv;
    BaseBuilt.unregister BaseBuilt.BDoc cs.cs_name
  
end


# 4696 "setup.ml"
module CustomPlugin = struct
# 0 "/home/gildor/programmation/oasis/src/plugins/custom/CustomPlugin.ml"
  
  (** Generate custom configure/build/doc/test/install system
      @author
    *)
  
  open BaseEnv
  open OASISGettext
  open OASISTypes
  
  
  
  type t =
      {
        cmd_main:      command_line conditional;
        cmd_clean:     (command_line option) conditional;
        cmd_distclean: (command_line option) conditional;
      } 
  
  let run  = BaseCustom.run 
  
  let main t _ extra_args =
    let cmd, args =
      var_choose t.cmd_main
    in
      run cmd args extra_args 
  
  let clean t pkg extra_args =
    match var_choose t.cmd_clean with
      | Some (cmd, args) ->
          run cmd args extra_args
      | _ ->
          ()
  
  let distclean t pkg extra_args =
    match var_choose t.cmd_distclean with
      | Some (cmd, args) ->
          run cmd args extra_args
      | _ ->
          ()
  
  module Build =
  struct 
    let main t pkg extra_args =
      main t pkg extra_args;
      List.fold_left
        (fun () sct ->
           let evs =
             match sct with 
               | Library (cs, bs, lib) when var_choose bs.bs_build ->
                   begin
                     let evs, _ = 
                       BaseBuilt.of_library 
                         BaseFilePath.of_unix
                         (cs, bs, lib) 
                     in
                       evs
                   end
               | Executable (cs, bs, exec) when var_choose bs.bs_build ->
                   begin
                     let evs, _, _ =
                       BaseBuilt.of_executable
                         BaseFilePath.of_unix
                         (cs, bs, exec)
                     in
                       evs
                   end
               | _ ->
                   []
           in
             List.iter
               (fun (bt, bnm, lst) -> BaseBuilt.register bt bnm lst)
               evs)
        ()
        pkg.sections
  
    let clean t pkg extra_args =
      clean t pkg extra_args;
      BaseBuilt.clean_all pkg
  
    let distclean t pkg extra_args =
      distclean t pkg extra_args
  end
  
  module Test =
  struct
    let main t pkg (cs, test) extra_args =
      try
        main t pkg extra_args;
        0.0
      with Failure s ->
        OASISMessage.warning 
          (f_ "Test '%s' fails: %s")
          cs.cs_name
          s;
        1.0
  
    let clean t pkg (cs, test) extra_args =
      clean t pkg extra_args
  
    let distclean t pkg (cs, test) extra_args =
      distclean t pkg extra_args 
  end
  
  module Doc =
  struct
    let main t pkg (cs, _) extra_args =
      main t pkg extra_args
  
    let clean t pkg (cs, _) extra_args =
      clean t pkg extra_args
  
    let distclean t pkg (cs, _) extra_args =
      distclean t pkg extra_args
  end
  
end


# 4817 "setup.ml"
open OASISTypes;;
let setup () =
  BaseSetup.setup
    {
       BaseSetup.configure = InternalConfigurePlugin.configure;
       build = OCamlbuildPlugin.build;
       test =
         [
            ("main",
              CustomPlugin.Test.main
                {
                   CustomPlugin.cmd_main =
                     [(EBool true, ("$(utoh \"../_build/test/test\")", []))];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   })
         ];
       doc =
         [
            ("manual",
              CustomPlugin.Doc.main
                {
                   CustomPlugin.cmd_main = [];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   });
            ("oasis",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/oasis";
                   modules =
                     [
                        "FormatExt";
                        "OASISGettext";
                        "OASISMessage";
                        "ODNFunc";
                        "PropList";
                        "OASIS";
                        "OASISBuildSection";
                        "OASISDocumentation";
                        "OASISExecutable";
                        "OASISExpr";
                        "OASISFlag";
                        "OASISHelp";
                        "OASISLibrary";
                        "OASISPackage";
                        "OASISPlugin";
                        "OASISSourceRepository";
                        "OASISTest";
                        "OASISTypes";
                        "OASISUnixPath";
                        "OASISUtils";
                        "OASISValues";
                        "OASISVersion";
                        "OASISData"
                     ];
                   });
            ("base",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/base";
                   modules =
                     [
                        "BaseArgExt";
                        "BaseBuilt";
                        "BaseCheck";
                        "BaseCustom";
                        "BaseData";
                        "BaseDev";
                        "BaseDoc";
                        "BaseEnvLight";
                        "BaseEnv";
                        "BaseExec";
                        "BaseFileAB";
                        "BaseFileGenerate";
                        "BaseFilePath";
                        "BaseFileUtil";
                        "BaseGenerate";
                        "BaseLog";
                        "BaseOCamlcConfig";
                        "BaseSetup";
                        "BaseStandardVar";
                        "BaseTest"
                     ];
                   });
            ("plugin-stdfiles",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/stdfiles";
                   modules = ["StdFilesPlugin"];
                   });
            ("plugin-ocamlbuild",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/ocamlbuild";
                   modules = ["OCamlbuildPlugin"; "OCamlbuildDocPlugin"];
                   });
            ("plugin-none",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/none";
                   modules = ["NonePlugin"];
                   });
            ("plugin-meta",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/META";
                   modules = ["METAPlugin"];
                   });
            ("plugin-internal",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/internal";
                   modules =
                     ["InternalConfigurePlugin"; "InternalInstallPlugin"];
                   });
            ("plugin-devfiles",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/devfiles";
                   modules = ["DevFilesPlugin"];
                   });
            ("plugin-custom",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src/plugins/custom";
                   modules = ["CustomPlugin"];
                   });
            ("builtin-plugins",
              OCamlbuildDocPlugin.doc_build
                {
                   OCamlbuildDocPlugin.path = "src";
                   modules = ["OASISBuiltinPlugins"];
                   })
         ];
       install = InternalInstallPlugin.install;
       uninstall = InternalInstallPlugin.uninstall;
       clean = [OCamlbuildPlugin.clean];
       clean_test =
         [
            ("main",
              CustomPlugin.Test.clean
                {
                   CustomPlugin.cmd_main =
                     [(EBool true, ("$(utoh \"../_build/test/test\")", []))];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   })
         ];
       clean_doc =
         [
            ("manual",
              CustomPlugin.Doc.clean
                {
                   CustomPlugin.cmd_main = [];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   });
            ("oasis",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/oasis";
                   modules =
                     [
                        "FormatExt";
                        "OASISGettext";
                        "OASISMessage";
                        "ODNFunc";
                        "PropList";
                        "OASIS";
                        "OASISBuildSection";
                        "OASISDocumentation";
                        "OASISExecutable";
                        "OASISExpr";
                        "OASISFlag";
                        "OASISHelp";
                        "OASISLibrary";
                        "OASISPackage";
                        "OASISPlugin";
                        "OASISSourceRepository";
                        "OASISTest";
                        "OASISTypes";
                        "OASISUnixPath";
                        "OASISUtils";
                        "OASISValues";
                        "OASISVersion";
                        "OASISData"
                     ];
                   });
            ("base",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/base";
                   modules =
                     [
                        "BaseArgExt";
                        "BaseBuilt";
                        "BaseCheck";
                        "BaseCustom";
                        "BaseData";
                        "BaseDev";
                        "BaseDoc";
                        "BaseEnvLight";
                        "BaseEnv";
                        "BaseExec";
                        "BaseFileAB";
                        "BaseFileGenerate";
                        "BaseFilePath";
                        "BaseFileUtil";
                        "BaseGenerate";
                        "BaseLog";
                        "BaseOCamlcConfig";
                        "BaseSetup";
                        "BaseStandardVar";
                        "BaseTest"
                     ];
                   });
            ("plugin-stdfiles",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/stdfiles";
                   modules = ["StdFilesPlugin"];
                   });
            ("plugin-ocamlbuild",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/ocamlbuild";
                   modules = ["OCamlbuildPlugin"; "OCamlbuildDocPlugin"];
                   });
            ("plugin-none",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/none";
                   modules = ["NonePlugin"];
                   });
            ("plugin-meta",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/META";
                   modules = ["METAPlugin"];
                   });
            ("plugin-internal",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/internal";
                   modules =
                     ["InternalConfigurePlugin"; "InternalInstallPlugin"];
                   });
            ("plugin-devfiles",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/extra/devfiles";
                   modules = ["DevFilesPlugin"];
                   });
            ("plugin-custom",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src/plugins/custom";
                   modules = ["CustomPlugin"];
                   });
            ("builtin-plugins",
              OCamlbuildDocPlugin.doc_clean
                {
                   OCamlbuildDocPlugin.path = "src";
                   modules = ["OASISBuiltinPlugins"];
                   })
         ];
       distclean = [];
       distclean_test =
         [
            ("main",
              CustomPlugin.Test.distclean
                {
                   CustomPlugin.cmd_main =
                     [(EBool true, ("$(utoh \"../_build/test/test\")", []))];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   })
         ];
       distclean_doc =
         [
            ("manual",
              CustomPlugin.Doc.distclean
                {
                   CustomPlugin.cmd_main = [];
                   cmd_clean = [(EBool true, None)];
                   cmd_distclean = [(EBool true, None)];
                   })
         ];
       package =
         {
            oasis_version = VInt (0, VInt (1, VEnd));
            ocaml_version =
              Some (VGreaterEqual (VInt (3, VInt (10, VInt (2, VEnd)))));
            findlib_version = None;
            name = "oasis";
            version = VInt (0, VInt (1, VInt (0, VEnd)));
            license = LGPL_link_exn;
            license_file = Some "LICENSE.txt";
            copyrights = ["(C) 2008-2010 OCamlCore SARL"];
            maintainers = [];
            authors = ["Sylvain Le Gall"];
            homepage = Some "http://oasis.forge.ocamlcore.org/";
            synopsis =
              "Architecture for building OCaml libraries and applications";
            description =
              Some
                "OASIS generates a full configure, build and install system for your application. It starts with a simple `_oasis` file at the toplevel of your project and creates everything required.\nIt uses external tools like OCamlbuild and it can be considered as the glue between various subsystems that do the job. It should support the following tools: - OCamlbuild - OMake (todo) - OCamlMakefile (todo), - ocaml-autoconf (todo)\nIt also features a do-it-yourself command line invocation and an internal configure/install scheme. Libraries are managed through findlib. It has been tested on GNU Linux and Windows.\nIt also allows to have standard entry points and description. It helps to integrates your libraries and software with third parties tools like GODI. \n";
            categories = [];
            conf_type =
              ("internal", Some (VInt (0, VInt (1, VInt (0, VEnd)))));
            conf_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command = [(EBool true, None)];
                 };
            build_type =
              ("ocamlbuild", Some (VInt (0, VInt (1, VInt (0, VEnd)))));
            build_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command =
                   [
                      (EBool true, None);
                      (EFlag "gettext",
                        Some (("$make", ["-C"; "po"; "all"])))
                   ];
                 };
            install_type =
              ("internal", Some (VInt (0, VInt (1, VInt (0, VEnd)))));
            install_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command = [(EBool true, None)];
                 };
            uninstall_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command = [(EBool true, None)];
                 };
            clean_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command =
                   [
                      (EBool true, None);
                      (EFlag "gettext",
                        Some (("$make", ["-C"; "po"; "clean"])))
                   ];
                 };
            distclean_custom =
              {
                 pre_command = [(EBool true, None)];
                 post_command = [(EBool true, None)];
                 };
            files_ab = [];
            sections =
              [
                 Executable
                   ({cs_name = "ocamlmod"; cs_data = PropList.Data.create (); 
                    },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src/tools";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage ("fileutils", None);
                             FindlibPackage ("str", None)
                          ];
                        bs_build_tools =
                          [ExternalTool "ocamlbuild"; ExternalTool "make"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {exec_custom = false; exec_main_is = "ocamlmod.ml"; });
                 Library
                   ({cs_name = "oasis"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src/oasis";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage ("fileutils", None);
                             FindlibPackage ("unix", None);
                             FindlibPackage ("str", None);
                             FindlibPackage ("extlib", None);
                             FindlibPackage ("findlib", None);
                             FindlibPackage ("odn", None);
                             FindlibPackage ("ocamlgraph", None)
                          ];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlyacc";
                             ExternalTool "ocamllex";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules =
                          [
                             "FormatExt";
                             "OASISGettext";
                             "OASISMessage";
                             "ODNFunc";
                             "PropList";
                             "OASIS";
                             "OASISBuildSection";
                             "OASISDocumentation";
                             "OASISExecutable";
                             "OASISExpr";
                             "OASISFlag";
                             "OASISHelp";
                             "OASISLibrary";
                             "OASISPackage";
                             "OASISPlugin";
                             "OASISSourceRepository";
                             "OASISTest";
                             "OASISTypes";
                             "OASISUnixPath";
                             "OASISUtils";
                             "OASISValues";
                             "OASISVersion";
                             "OASISData"
                          ];
                        lib_internal_modules =
                          [
                             "OASISVersion_lexer";
                             "OASISVersion_parser";
                             "OASISSection";
                             "OASISSchema";
                             "OASISConf";
                             "OASISHelpData";
                             "OASISAst";
                             "OASISCheck";
                             "OASISRecDescParser";
                             "OASISCustom";
                             "OASISQuickstart"
                          ];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({cs_name = "base"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src/base";
                        bs_compiled_object = Byte;
                        bs_build_depends = [InternalLibrary "oasis"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules =
                          [
                             "BaseArgExt";
                             "BaseBuilt";
                             "BaseCheck";
                             "BaseCustom";
                             "BaseData";
                             "BaseDev";
                             "BaseDoc";
                             "BaseEnvLight";
                             "BaseEnv";
                             "BaseExec";
                             "BaseFileAB";
                             "BaseFileGenerate";
                             "BaseFilePath";
                             "BaseFileUtil";
                             "BaseGenerate";
                             "BaseLog";
                             "BaseOCamlcConfig";
                             "BaseSetup";
                             "BaseStandardVar";
                             "BaseTest"
                          ];
                        lib_internal_modules = [];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-stdfiles";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/extra/stdfiles";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["StdFilesPlugin"];
                        lib_internal_modules = ["StdFilesData"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-ocamlbuild";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/ocamlbuild";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules =
                          ["OCamlbuildPlugin"; "OCamlbuildDocPlugin"];
                        lib_internal_modules =
                          [
                             "OCamlbuildData";
                             "OCamlbuildCommon";
                             "OCamlbuildId";
                             "OCamlbuildBase";
                             "OCamlbuildFindlib"
                          ];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-none";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/none";
                        bs_compiled_object = Byte;
                        bs_build_depends = [InternalLibrary "oasis"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["NonePlugin"];
                        lib_internal_modules = ["NoneData"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-meta";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/extra/META";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["METAPlugin"];
                        lib_internal_modules = ["METAData"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-internal";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/internal";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules =
                          ["InternalConfigurePlugin"; "InternalInstallPlugin"
                          ];
                        lib_internal_modules = ["InternalData"; "InternalId"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-devfiles";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/extra/devfiles";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["DevFilesPlugin"];
                        lib_internal_modules = ["DevFilesData"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "plugin-custom";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src/plugins/custom";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [InternalLibrary "oasis"; InternalLibrary "base"];
                        bs_build_tools =
                          [
                             ExternalTool "ocamlbuild";
                             ExternalTool "make";
                             InternalExecutable "ocamlmod";
                             ExternalTool "ocamlify"
                          ];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["CustomPlugin"];
                        lib_internal_modules = ["CustomData"];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Library
                   ({
                       cs_name = "builtin-plugins";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "src";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             InternalLibrary "plugin-none";
                             InternalLibrary "plugin-internal";
                             InternalLibrary "plugin-ocamlbuild";
                             InternalLibrary "plugin-custom";
                             InternalLibrary "plugin-meta";
                             InternalLibrary "plugin-devfiles";
                             InternalLibrary "plugin-stdfiles"
                          ];
                        bs_build_tools =
                          [ExternalTool "ocamlbuild"; ExternalTool "make"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {
                        lib_modules = ["OASISBuiltinPlugins"];
                        lib_internal_modules = [];
                        lib_findlib_parent = None;
                        lib_findlib_name = None;
                        lib_findlib_containers = [];
                        });
                 Executable
                   ({cs_name = "test"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, false)];
                        bs_path = "test";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             FindlibPackage ("oUnit", None);
                             InternalLibrary "oasis";
                             InternalLibrary "base";
                             InternalLibrary "builtin-plugins"
                          ];
                        bs_build_tools =
                          [ExternalTool "ocamlbuild"; ExternalTool "make"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {exec_custom = false; exec_main_is = "test.ml"; });
                 Executable
                   ({cs_name = "OASIS"; cs_data = PropList.Data.create (); },
                     {
                        bs_build = [(EBool true, true)];
                        bs_install = [(EBool true, true)];
                        bs_path = "src";
                        bs_compiled_object = Byte;
                        bs_build_depends =
                          [
                             InternalLibrary "oasis";
                             InternalLibrary "base";
                             InternalLibrary "builtin-plugins"
                          ];
                        bs_build_tools =
                          [ExternalTool "ocamlbuild"; ExternalTool "make"];
                        bs_c_sources = [];
                        bs_data_files = [];
                        bs_ccopt = [(EBool true, [])];
                        bs_cclib = [(EBool true, [])];
                        bs_dlllib = [(EBool true, [])];
                        bs_dllpath = [(EBool true, [])];
                        bs_byteopt = [(EBool true, [])];
                        bs_nativeopt = [(EBool true, [])];
                        },
                     {exec_custom = false; exec_main_is = "OASISMain.ml"; });
                 Flag
                   ({cs_name = "gettext"; cs_data = PropList.Data.create (); 
                    },
                     {
                        flag_description = Some "Use ocaml-gettext for i18n";
                        flag_default = [(EBool true, true)];
                        });
                 Test
                   ({cs_name = "main"; cs_data = PropList.Data.create (); },
                     {
                        test_type =
                          ("custom",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        test_command =
                          [
                             (EBool true,
                               ("$(utoh \"../_build/test/test\")", []))
                          ];
                        test_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        test_working_directory = Some "test";
                        test_run = [(EBool true, true)];
                        test_tools =
                          [
                             ExternalTool "make";
                             InternalExecutable "OASIS";
                             InternalExecutable "test"
                          ];
                        });
                 Doc
                   ({cs_name = "manual"; cs_data = PropList.Data.create (); },
                     {
                        doc_type =
                          ("custom",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, true)];
                        doc_install_dir = "$docdir";
                        doc_data_files = [];
                        doc_build_tools = [ExternalTool "make"];
                        });
                 Doc
                   ({cs_name = "oasis"; cs_data = PropList.Data.create (); },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, true)];
                        doc_install_dir = "$htmldir/oasis";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({cs_name = "base"; cs_data = PropList.Data.create (); },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, true)];
                        doc_install_dir = "$htmldir/base";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-stdfiles";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-stdfiles";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-ocamlbuild";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-ocamlbuild";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-none";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-none";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-meta";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-meta";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-internal";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-internal";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-devfiles";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-devfiles";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "plugin-custom";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/plugin-custom";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        });
                 Doc
                   ({
                       cs_name = "builtin-plugins";
                       cs_data = PropList.Data.create ();
                       },
                     {
                        doc_type =
                          ("ocamlbuild",
                            Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                        doc_custom =
                          {
                             pre_command = [(EBool true, None)];
                             post_command = [(EBool true, None)];
                             };
                        doc_build = [(EBool true, true)];
                        doc_install = [(EBool true, false)];
                        doc_install_dir = "$htmldir/builtin-plugins";
                        doc_data_files = [];
                        doc_build_tools =
                          [ExternalTool "ocamldoc"; ExternalTool "ocamlbuild"
                          ];
                        })
              ];
            plugins =
              [
                 ("DevFiles", Some (VInt (0, VInt (1, VInt (0, VEnd)))));
                 ("StdFiles", Some (VInt (0, VInt (1, VInt (0, VEnd)))))
              ];
            schema_data = PropList.Data.create ();
            };
       }
  ;;
(* OASIS_STOP *)
let () = setup ();;
