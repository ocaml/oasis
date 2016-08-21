(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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

(** File System functions

    @author Sylvain Le Gall
*)

type 'a filename = string

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
    method string_of_filename: 'a filename -> string
    method open_out: ?mode:(open_flag list) -> ?perm:int -> 'a filename -> writer
    method open_in: ?mode:(open_flag list) -> ?perm:int -> 'a filename -> reader
    method file_exists: 'a filename -> bool
    method remove: 'a filename -> unit
  end


module Mode =
struct
  let default_in = [Open_rdonly]
  let default_out = [Open_wronly; Open_creat; Open_trunc]

  let text_in = Open_text :: default_in
  let text_out = Open_text :: default_out

  let binary_in = Open_binary :: default_in
  let binary_out = Open_binary :: default_out
end

let std_length = 4096 (* Standard buffer/read length. *)
let binary_out = Mode.binary_out
let binary_in = Mode.binary_in

let of_unix_filename ufn = (ufn: 'a filename)
let to_unix_filename fn = (fn: string)


let defer_close o f =
  try
    let r = f o in o#close; r
  with e ->
    o#close; raise e


let stream_of_reader rdr =
  let buf = Buffer.create std_length in
  let pos = ref 0 in
  let eof = ref false in
  let rec next idx =
    let bpos = idx - !pos in
    if !eof then begin
      None
    end else if bpos < Buffer.length buf then begin
      Some (Buffer.nth buf bpos)
    end else begin
      pos := !pos + Buffer.length buf;
      Buffer.clear buf;
      begin
        try
          rdr#input buf std_length;
        with End_of_file ->
          if Buffer.length buf = 0 then
            eof := true
      end;
      next idx
    end
  in
  Stream.from next


let read_all buf rdr =
  try
    while true do
      rdr#input buf std_length
    done
  with End_of_file ->
    ()

class ['a] host_fs rootdir : ['a] fs =
  object (self)
    method private host_filename fn = Filename.concat rootdir fn
    method string_of_filename = self#host_filename

    method open_out ?(mode=Mode.text_out)  ?(perm=0o666) fn =
      let chn = open_out_gen mode perm (self#host_filename fn) in
      object
        method close = close_out chn
        method output buf = Buffer.output_buffer chn buf
      end

    method open_in ?(mode=Mode.text_in) ?(perm=0o666) fn =
      (* TODO: use Buffer.add_channel when minimal version of OCaml will
       * be >= 4.03.0 (previous version was discarding last chars).
       *)
      let chn = open_in_gen mode perm (self#host_filename fn) in
      let strm = Stream.of_channel chn in
      object
        method close = close_in chn
        method input buf len =
          let read = ref 0 in
          try
            for _i = 0 to len do
              Buffer.add_char buf (Stream.next strm);
              incr read
            done
          with Stream.Failure ->
            if !read = 0 then
              raise End_of_file
      end

    method file_exists fn = Sys.file_exists (self#host_filename fn)
    method remove fn = Sys.remove (self#host_filename fn)
  end

