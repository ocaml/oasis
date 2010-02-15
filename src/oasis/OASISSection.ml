
(** Manipulate section 
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

let section_fields _ _ =
  fun nm data ->
    {
      cs_name = nm;
      cs_data = data;
    }
                             (*
type ('a, 'b) fold =
    'a -> section -> 'b -> 'a

type 'a t =
    {
      fold_library:    ('a, (build_section * library)) fold;
      fold_executable: ('a, (build_section * executable)) fold;
      fold_flag:       ('a, flag) fold;
      fold_src_repo:   ('a, source_repository) fold;
      fold_test:       ('a, test) fold;
    }

type section_kind = 
  | SKLibrary 
  | SKExecutable
  | SKFlag
  | SKSrcRepo
  | SKTest

let default =
  let fold_ignore a _ _ =
    a
  in
    {
      fold_library    = fold_ignore;
      fold_executable = fold_ignore;
      fold_flag       = fold_ignore;
      fold_src_repo   = fold_ignore;
      fold_test       = fold_ignore;
    }

let fold_section t acc pkg =
  List.fold_left
    (fun acc section ->
       match section.sct_spec with
         | Library (bs, lib) ->
             t.fold_library acc section (bs, lib)
         | Executable (bs, exec) ->
             t.fold_executable acc section (bs, exec)
         | Flag flag ->
             t.fold_flag acc section flag
         | SrcRepo src_repo ->
             t.fold_src_repo acc section src_repo
         | Test tst ->
             t.fold_test acc section tst)
    acc
    pkg.sections

let find_section knd nm pkg = 
  let search_t =
    let choose knd2 = 
      if knd = knd2 then
        (fun acc sect _ -> sect :: acc)
      else
        (fun acc _ _ -> acc)
    in
      {
        fold_library    = choose SKLibrary;
        fold_executable = choose SKExecutable;
        fold_flag       = choose SKFlag;
        fold_src_repo   = choose SKSrcRepo;
        fold_test       = choose SKTest;
      }
  in
  let lst =
    fold_section
      search_t
      []
      pkg
  in
    match lst with 
      | hd :: _ ->
          hd
      | [] ->
          raise Not_found

let find_test nm pkg =
  let lst =
    fold_section
      {default with
           fold_test = (fun acc section test -> 
                          (section, test) :: acc);}
      []
      pkg
  in
    match lst with 
      | hd :: _ -> hd
      | [] -> raise Not_found
                              *)
