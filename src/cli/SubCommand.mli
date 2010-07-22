
(** SubCommand definition 
    @author Sylvain Le Gall
  *)

open OASISTypes

type t = 
    {
      scmd_name:     name;   (** Name of the subcommand, used to call it *)
      scmd_synopsis: string; (** Short description of the subcommnad,
                                 displayed when doing a summary of the
                                 available subcommands
                               *)
      scmd_help:     string; (** Long description of the subcommand,
                                 displayed when showing help of the 
                                 subcommand.

                                 It can contains variable substitution as
                                 defined in [Buffer.add_substitute].
                               *)
      scmd_specs:    (Arg.key * Arg.spec * Arg.doc) list;
                                     (** [Arg] spec list *)
      scmd_usage:    string;         (** [Arg] usage text *)
      scmd_anon:     string -> unit; (** [Arg] anon function *)
      scmd_main:     unit -> unit; (** Real action of the subcommand *)
    }

(** [make ~std_usage name synopsis help main] Create a subcommand using
    provided data, see {!t} for their meanings. If [~std_usage] is set
    use ["[options*]"] for it. Fields that are not defined by make use
    a sane default.
  *)
val make :
  ?std_usage:bool -> 
  name -> 
  string -> 
  string -> 
  (unit -> unit) -> 
  t

(** Register a subcommand.
  *)
val register : t -> unit

(** Iterate through subcommands.
  *)
val fold : (t -> 'a -> 'a) -> 'a -> 'a

(** Find a subcommand.
  *)
val find : name -> t
