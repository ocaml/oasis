type 'a filename

val of_unix_filename: OASISUnixPath.unix_filename -> 'a filename

class type closer =
  object
    method close: unit
  end

class type reader =
  object
    inherit closer
    method input: Buffer.t -> int -> unit
  end

class type writer =
  object
    inherit closer
    method output: Buffer.t -> unit
  end

class type ['a] fs =
  object
    (** Return a string representation of the filename. It is may not be a real
        host filename.
    *)
    method string_of_filename: 'a filename -> string

    method open_out:
      ?mode:(open_flag list) -> ?perm:int -> 'a filename -> writer

    method open_in:
      ?mode:(open_flag list) -> ?perm:int -> 'a filename -> reader

    method file_exists: 'a filename -> bool

    method remove: 'a filename -> unit
  end

val defer_close: (#closer as 'a) -> ('a -> 'b) -> 'b

val binary_out: open_flag list
val binary_in: open_flag list

val stream_of_reader: #reader -> char Stream.t

val read_all: Buffer.t -> #reader -> unit

class ['a] host_fs: OASISUnixPath.host_filename -> ['a] fs
