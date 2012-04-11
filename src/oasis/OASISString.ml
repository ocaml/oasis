


(** Various string utilities.
   
    Mostly inspired by extlib and batteries ExtString and BatString libraries.

    @author Sylvain Le Gall
  *)

(** [nsplit c s] Split the string [s] at char [c]. It doesn't include the
    separator.
  *)
let nsplit str c =
  if str = "" then
    []
  else
    let buf = Buffer.create 13 in
    let lst = ref [] in
    let push () =
      lst := Buffer.contents buf :: !lst;
      Buffer.clear buf
    in
    let str_len = String.length str in
      for i = 0 to str_len - 1 do
        if str.[i] = c then
          push ()
        else
          Buffer.add_char buf str.[i]
      done;
      push ();
      List.rev !lst

let find ~what ?(offset=0) str =
  let what_idx = ref 0 in
  let str_idx = ref offset in 
    while !str_idx < String.length str && 
          !what_idx < String.length what do
      if str.[!str_idx] = what.[!what_idx] then
        incr what_idx
      else
        what_idx := 0;
      incr str_idx
    done;
    if !what_idx <> String.length what then
      raise Not_found
    else 
      !str_idx - !what_idx

let sub_start str len = 
  let str_len = String.length str in
  if len >= str_len then
    ""
  else
    String.sub str len (str_len - len)

let sub_end ?(offset=0) str len =
  let str_len = String.length str in
  if len >= str_len then
    ""
  else
    String.sub str 0 (str_len - len)

let starts_with ~what ?(offset=0) str =
  let what_idx = ref 0 in
  let str_idx = ref offset in
  let ok = ref true in
    while !ok &&
          !str_idx < String.length str && 
          !what_idx < String.length what do
      if str.[!str_idx] = what.[!what_idx] then
        incr what_idx
      else
        ok := false;
      incr str_idx
    done;
    if !what_idx = String.length what then
      true
    else 
      false

let strip_starts_with ~what str =
  if starts_with ~what str then
    sub_start str (String.length what)
  else
    raise Not_found

let ends_with ~what ?(offset=0) str =
  let what_idx = ref ((String.length what) - 1) in
  let str_idx = ref ((String.length str) - 1) in
  let ok = ref true in
    while !ok &&
          offset <= !str_idx && 
          0 <= !what_idx do
      if str.[!str_idx] = what.[!what_idx] then
        decr what_idx
      else
        ok := false;
      decr str_idx
    done;
    if !what_idx = -1 then
      true
    else 
      false

let strip_ends_with ~what str =
  if ends_with ~what str then
    sub_end str (String.length what)
  else
    raise Not_found

let replace_chars f s =
  let buf = String.make (String.length s) 'X' in
    for i = 0 to String.length s - 1 do
      buf.[i] <- f s.[i]
    done;
    buf

(* END EXPORT *)

let is_whitespace =
  function
    | ' ' | '\r' | '\n' | '\t' -> true
    |  _  -> false

let tokenize ?(is_whitespace=is_whitespace) ?(tokens=[]) str =
  let lst = ref [] in
  let buf = Buffer.create 13 in
  let idx = ref 0 in    
  let push () = 
    (* Push the content of the buffer on the list. *)
    if Buffer.length buf > 0 then
      begin
        lst := Buffer.contents buf :: !lst;
        Buffer.clear buf
      end
  in
  let match_token () = 
    List.exists
      (fun tok ->
         if starts_with ~what:tok ~offset:!idx str then
           begin
             push ();
             lst := tok :: !lst;
             idx := !idx + (String.length tok);
             true
           end
         else
           false)
      tokens
  in
    while !idx < String.length str do
      let c = str.[!idx] in
      if is_whitespace c then
        begin
          push ();
          incr idx
        end
      else if match_token () then
        begin
          ()
        end
      else
        begin
          Buffer.add_char buf c;
          incr idx
        end
    done;
    push ();
    List.rev !lst

let tokenize_genlex ?(tokens=[]) str =
  let strm = Genlex.make_lexer tokens (Stream.of_string str) in
  let lst = ref [] in
    Stream.iter (fun tok -> lst := tok :: !lst) strm;
    List.rev !lst

let split str c =
  let idx = String.index str c in
    String.sub str 0 idx,
    String.sub str (idx + 1) (String.length str - idx - 1)

let trim str =
  let start_non_blank = ref 0 in
  let stop_non_blank = ref ((String.length str) - 1) in
    while !start_non_blank < String.length str &&
          is_whitespace (str.[!start_non_blank]) do
      incr start_non_blank
    done;
    while !start_non_blank <= !stop_non_blank &&
          is_whitespace (str.[!stop_non_blank]) do
      decr stop_non_blank
    done;
    String.sub str !start_non_blank (!stop_non_blank - !start_non_blank + 1)

let fold_left f acc str =
  let racc = ref acc in
    for i = 0 to String.length str - 1 do
      racc := f !racc str.[i]
    done;
    !racc
