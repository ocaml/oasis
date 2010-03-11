
(** OASIS library

    @author Sylvain Le Gall
  *)  

open OASISTypes

(** Default configuration for parser/checker *)
let default_conf =
  {
    oasisfn        = None;
    srcdir         = None;
    ignore_unknown = false;
    debug          = false;
  }

(** [from_file ~conf fn] Parse the OASIS file [fn] and check it using
    context [conf].
  *)
let from_file ?(conf=default_conf) fn = 
  let conf =
    (* Add srcdir information to configuration *)
    match conf.srcdir with 
      | Some _ -> conf
      | None -> {conf with srcdir = Some (Filename.dirname fn)}
  in
  let conf =
    (* Add OASIS filename information to configuration *)
    match conf.oasisfn with
      | Some _ -> conf
      | None -> {conf with oasisfn = Some fn}
  in
  let chn =
    open_in fn
  in
  let pkg = 
    OASISAst.to_package 
      conf
      (Stream.of_channel chn)
  in
    close_in chn;
    pkg

(** [from_string ~conf str] Parse the OASIS string [str] and check it using
    context [conf].
  *)
let from_string ?(conf=default_conf) str =
  OASISAst.to_package 
    conf
    (Stream.of_string str)

(** Print help about OASIS fields.
  *)
let pp_help = OASISHelp.pp_help

