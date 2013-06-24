
open OUnit

let verbose = ref false

let pluginloader = ref "false"

let datadir =
  FilePath.make_filename ["src"; "ext"; "plugin-loader"; "test"; "data"]

let findlibdir =
  FilePath.concat datadir "findlib"

let bracket_tmpdir f =
  bracket
    (fun () ->
       let fn = Filename.temp_file "plugin-loader-" ".dir" in
         FileUtil.rm [fn];
         FileUtil.mkdir fn;
         fn)
    f
    (fun fn ->
       FileUtil.rm ~recurse:true [fn]) 

let bracket_findlib f =
  bracket_tmpdir
    (fun dn ->
       let builddir =
         FilePath.make_absolute (FileUtil.pwd ()) "_build"
       in
       let buildfindlibdir =
         FilePath.concat builddir findlibdir
       in
       let findlibdir =
         FilePath.make_absolute (FileUtil.pwd ()) findlibdir
       in
       let copy_to_dn fn_dir fn =
         let tgt = FilePath.reparent fn_dir dn fn in
           FileUtil.mkdir ~parent:true (FilePath.dirname tgt);
           if !verbose then
             Printf.eprintf
               "I: Copy file '%s' to '%s'\n%!"
               fn tgt;
           FileUtil.cp [fn] tgt
       in
       (* Find all the .mlldir in findlibdir and copy .cma/.cmxs from
        * the _build dir and then all the META.
        *)
       FileUtil.find
         (FileUtil.Or
            (FileUtil.Has_extension "cma",
             FileUtil.Has_extension "cmxs"))
         buildfindlibdir
         (fun () fn ->
            copy_to_dn buildfindlibdir fn)
         ();
       FileUtil.find
         (FileUtil.Basename_is "META")
         findlibdir
         (fun () fn ->
            copy_to_dn findlibdir fn)
         ();
       f dn)

let assert_pluginloader dn args =
  let buf = Buffer.create 13 in
  let lst = ref [] in
  let ocamlpath = 
    try
      FilePath.string_of_path
        ((FilePath.path_of_string (Sys.getenv "OCAMLPATH")) @ [dn])
    with Not_found ->
      dn
  in
  let env =
    Array.append
      [|"OCAMLPATH="^ocamlpath|]
      (Unix.environment ())
  in
    assert_command
      ~env
      ~use_stderr:true
      ~foutput:(Stream.iter
                  (function
                     | '\n' ->
                         if !verbose then
                           prerr_endline (Buffer.contents buf);
                         lst := Buffer.contents buf :: !lst;
                         Buffer.clear buf
                     | c ->
                         Buffer.add_char buf c))
      !pluginloader args;

    if !verbose then 
      prerr_endline (Buffer.contents buf);
    List.rev (Buffer.contents buf :: !lst)

let _lst : test_result list  =
  run_test_tt_main
    ~arg_specs:["--pluginloader",
                Arg.Set_string pluginloader,
                "exec Set pluginloader executable.";
                "-not-long",
                Arg.Unit ignore,
                " Run long tests."]
    ~set_verbose:(fun b -> verbose := b)
    ("PluginLoader" >:::
     ["list" >::
      bracket_findlib
        (fun dn ->
           let lst = assert_pluginloader dn [] in
             assert_equal
               ~printer:(String.concat ", ")
               ["plugin1: first plugin";
                "plugin2: second plugin";
                "plugin3: third plugin";
                ""]
               lst);

      "load" >::
      bracket_findlib
        (fun dn ->
           let lst = assert_pluginloader dn ["-load"; "plugin1"] in
             assert_equal
               ~printer:(String.concat ", ")
               ["plugin_loaded: plugin1"; ""]
               lst)])
