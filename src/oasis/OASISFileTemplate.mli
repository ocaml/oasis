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


(** Generate files replacing parts of it

    This module allow to generate files using template. Each template files
    is split into three parts: an header, a body and a footer. We target at
    changing the body part. If target file already exists, we load the header
    and the footer from it. We merge the three parts and repalce the target
    files.

    There are some safety limits:
    - embed a digest to check nothing has changed in the body
    - if digest of the body has changed, create a backup
    - if we cannot find OASIS_START/OASIS_STOP body section, leave the
      file untouched

    The whole module is {b not exported}.

    @author Sylvain Le Gall
*)


open OASISUnixPath


(** {2 Comments} *)


(** Comment definition. *)
type comment


(** .ml comments.  *)
val comment_ml: comment


(** Shell comments.  *)
val comment_sh: comment


(** Makefile comments.  *)
val comment_makefile: comment


(** OCamlbuild comments.  *)
val comment_ocamlbuild: comment


(** .bat file comments.  *)
val comment_bat: comment


(** META file comments.  *)
val comment_meta: comment


(** Markdown comments. *)
val comment_markdown: comment


(** {2 Template} *)


type line = string


type body =
    NoBody
  | Body of line list
  | BodyWithDigest of Digest.t * line list


type template =
  {
    fn: host_filename;
    comment: comment;
    header: line list;
    body: body;
    footer: line list;
    perm: int;
    important: bool; (** Determine if should be kept in dynamic mode. *)
    disable_oasis_section: bool;
    (** Determine if OASIS section comments and digest should be omitted. *)
  }


(** [template_make fn cmt header body footer] Create a template for which
    target file is [fn].  *)
val template_make:
  host_filename ->
  comment -> line list -> line list -> line list -> template


(** [template_of_string_list ~ctxt ~template ~pure fn cmt lst] Split the list
    [lst] into a header, body and footer, using comment [cmt] to determine each
    part. Set [~template] if this is an embedded template (i.e. not a file
    loaded from disk). If [~disable_oasis_section] is set, then the list is
    processed on the assumption that there is no header and footer. See
    {!template_make} for other options.  *)
val template_of_string_list:
  ctxt:OASISContext.t -> template:bool ->
  ?disable_oasis_section:bool -> host_filename -> comment -> line list -> template


(** [template_of_ml_file fn] Create an OCaml file template taking into account
    subtleties, like line modifier. See {!template_make} for other options.
*)
val template_of_mlfile:
  host_filename -> line list -> line list -> line list -> template


(** {2 File generation} *)

(** Create a list representation of the file. *)
val to_string_list: template -> line list

(** Describe what has been done to generate a file out of a template.
*)
type file_generate_change =
    Create of host_filename
  (** [Create fn], [fn] is the target file, nothing exists before *)
  | Change of host_filename * host_filename option
  (** [Change (fn, bak)], [bak] is the backup file, an existing file
      has been changed. *)
  | NoChange
  (** Nothing done, the file doesn't need to be updated *)


(** Reset to pristine a generated file.
*)
val file_rollback: ctxt:OASISContext.t -> file_generate_change -> unit


(** Generate a file using a template. Only the part between OASIS_START and
    OASIS_STOP will be really replaced if the file exists. If the file doesn't
    exist use the whole template. If [~remove] is [true], then an existing file
    will be deleted iff the template body is [[]] and the header and footer of
    the file match the template's (used by the -remove option for setup-clean).
*)
val file_generate:
  ctxt:OASISContext.t ->
  ?remove:bool -> backup:bool -> template -> file_generate_change


(** {2 Multiple templates management } *)


(** Try to add a file that is already in the set
*)
exception AlreadyExists of host_filename


(** Set of templates.
*)
type templates


(** No generated template files with the given set of files with the OASIS
    section disabled. *)
val create: disable_oasis_section:unix_filename list -> unit -> templates


(** Find a generated template file.
*)
val find: host_filename -> templates -> template


(** Add a generated template file.
*)
val add: template -> templates -> templates


(** Remove a generated template file.
*)
val remove: host_filename -> templates -> templates


(** Add or replace a generated template file.
*)
val replace: template -> templates -> templates


(** Fold over generated template files.
*)
val fold: (template -> 'b -> 'b) -> templates -> 'b -> 'b
