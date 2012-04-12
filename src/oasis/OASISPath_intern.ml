
let fn_reader ?(os_type=Sys.os_type) fn =
  let fn_part_of_string =
    function
      | "."  -> `CurrentDir
      | ".." -> `ParentDir
      | str  -> `Component str
  in
  if os_type = "Unix" then
    begin
      let sep = '/' in
      let fbeg, fn' =
        try
          (fun lst -> (`Root "") :: lst),
          OASISString.strip_starts_with ~what:(String.make 1 sep) fn
        with Not_found ->
          (fun lst -> lst), fn
      in
        fbeg (List.map fn_part_of_string (OASISString.nsplit fn sep))
    end
  else if os_type = "Win32" then
    begin
      let lst =
        List.map fn_part_of_string
          (OASISString.nsplitf fn (fun c -> c = '\\' || c = '/'))
      in
        match lst with
          | `Component str :: tl ->
              begin
                try
                  let drive_letter, rmng = OASISString.split str ':' in
                    if rmng = "" then
                      (`Root drive_letter) :: tl
                    else
                      (`RootRelative drive_letter) :: (`Component rmng) :: tl
                with Not_found ->
                  lst
              end
          | lst ->
              lst
    end
  else
    invalid_arg "OASISHostPath.dir_reader"

let rec fn_reduce acc =
  function
    | ((`Root _) | (`RootRelative _)) as hd :: tl ->
        fn_reduce [hd] tl
    | (`CurrentDir | `Component "") :: tl ->
        fn_reduce acc tl
    | `Component _ :: `ParentDir :: tl ->
        fn_reduce [] (List.rev_append acc tl)
    | (`Component _ | `ParentDir) as hd :: tl ->
        fn_reduce (hd :: acc) tl
    | [] ->
        if acc = [] then
          [`CurrentDir]
        else
          List.rev acc
