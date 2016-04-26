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


(** Entry points for 'setup.ml'
    @author Sylvain Le Gall
*)


open OASISTypes


type std_args_fun =
  package -> arg array -> unit


type ('a, 'b) section_args_fun =
  name * (package -> (common_section * 'a) -> arg array -> 'b)


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

    oasis_fn:        string option;
    (** Filename of _oasis that matches the package field. *)

    oasis_version:   string;
    (** OASIS version that has generated this structure. *)

    oasis_digest:    Digest.t option;
    (** Digest of _oasis that matches the package field. *)

    oasis_exec:      string option;
    (** Name of oasis executable to use, only for testing. *)

    oasis_setup_args: string list;
    (** Args to use when updating the setup.ml. *)

    setup_update: bool;
    (** Are we allowed to update the setup.ml (eq. of -setup-update weak). *)
  }



(** Run the configure step.
*)
val configure: t -> arg array -> unit


(** Run the build step.
*)
val build: t -> arg array -> unit


(** Run the doc step: build all documents.
*)
val doc: t -> arg array -> unit


(** Run the test step: run all tests.
*)
val test: t -> arg array -> unit


(** Run the install step.
*)
val install: t -> arg array -> unit


(** Run the uninstall step.
*)
val uninstall: t -> arg array -> unit


(** Run the clean step.
*)
val clean: t -> arg array -> unit


(** Run the distclean step.
*)
val distclean: t -> arg array -> unit


(** Run the reinstall step: deinstall and install.
*)
val reinstall: t -> arg array -> unit


(** Run all steps: configure, build, doc, test and install.
*)
val all: t -> arg array -> unit


(** Display OASIS version used to generate this setup.ml
*)
val version: t -> arg array -> unit


(** The first function called when running 'setup.ml'.
*)
val setup: t -> unit


(** Default filename for '_oasis'.
*)
val default_oasis_fn: host_filename

(** Default filename for 'setup.ml'. {b Not exported}
*)
val default_filename: host_filename


(** Get template 'setup.ml' file out of the plugin context.
    {b Not exported}.
*)
val find: OASISPlugin.context_act -> OASISFileTemplate.template


(** Create [t] and plugin context from an OASIS package and the
    matching _oasis. {b Not exported}.
*)
val of_package:
  ?oasis_fn:host_filename ->
  ?oasis_exec:host_filename ->
  ?oasis_setup_args:string list ->
  setup_update:bool ->
  OASISSetupUpdate.t ->
  package ->
  OASISPlugin.context_act * t
