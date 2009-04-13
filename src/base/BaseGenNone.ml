
(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

open BaseGenerate;;

let no_generate knd data =
  {
    moduls           = [];
    pp_setup_fun     = (fun fmt _ -> 
                          Format.fprintf 
                            fmt
                            "@[fun _ ->@, @[failwith@, \
                              \"No implementation for %s\"@]@]"
                            (string_of_generator_kind knd));
    pp_clean_fun     = None;
    pp_distclean_fun = None;
    other_action     = ignore;
  },
  data
;;

List.iter
  (fun knd -> 
     generator_register 
       knd
       "none"
       (no_generate knd))
  [Build; Doc; Test; Install;]
;;

configure_generator_register
  "none"
  (fun _ ->
    {
      moduls           = [];
      pp_setup_fun     = (fun fmt _ -> 
                            Format.fprintf 
                              fmt
                              "@[fun _ ->@, @[failwith@, \
                                  \"No implementation for configure\"@]@]");
      pp_clean_fun     = None;
      pp_distclean_fun = None;
      other_action     = ignore;
    })
;;

