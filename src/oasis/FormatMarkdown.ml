
(** Markdown formatter 
    @author Sylvain Le Gall
  *)

open Format
open FormatExt

let pp_print_endblock ?check_last_char fmt () = 
  match check_last_char with
    | Some s ->
        begin
          if String.length s > 0 &&
             s.[(String.length s) - 1] = '\n' then
            begin
              pp_print_newline fmt ()
            end
          else
            begin
              pp_print_newline fmt ();
              pp_print_newline fmt ()
            end
        end
    | None ->
        begin
          pp_print_newline fmt ();
          pp_print_newline fmt ()
        end

let pp_print_title lvl fmt str =
  let pp_print_underlined c fmt str =
    pp_print_string fmt str;
    pp_print_newline fmt ();
    pp_print_string fmt (String.make (String.length str) c)
  in
    if lvl = 1 then
      pp_print_underlined '=' fmt str
    else if lvl = 2 then
      pp_print_underlined '-' fmt str
    else 
      begin
        (* ATX style *)
        pp_print_string fmt (String.make lvl '#');
        pp_print_string fmt str
      end;
    pp_print_endblock fmt ()

let pp_print_def fmt term defs = 
  pp_print_string fmt term;
  pp_print_newline fmt ();
  List.iter 
    (fun (pp_print_e, e) ->
       pp_print_string fmt ":   ";
       pp_open_box fmt 0;
       pp_print_e fmt e;
       pp_close_box fmt ();
       pp_print_newline fmt ())
    defs;
  pp_print_newline fmt ()

let pp_print_para fmt str =
  pp_open_box fmt 0;
  pp_print_string_spaced fmt str;
  pp_close_box fmt ();
  pp_print_endblock fmt ()
