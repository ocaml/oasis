
(** Clean generated template files
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand
open OASISFileTemplate
open OASISPlugin 

let replace_sections =
  ref false

let main () = 
  BaseGenerate.restore ();
  if !replace_sections then
    begin
      let ctxt, _ = 
        BaseSetup.of_package 
          (OASISParse.from_file 
             ~ctxt:!BaseContext.default
             !Setup.oasis_fn)
      in
        OASISFileTemplate.fold
          (fun tmpl () ->
             match tmpl.body with 
             | Body _ 
             | BodyWithDigest _ ->
                 begin
                   let _chng: file_generate_change =
                     file_generate 
                       ~ctxt:!BaseContext.default
                       ~backup:false 
                       {tmpl with body = Body []}
                   in
                     ()
                 end
             | NoBody ->
                 ())
          ctxt.files
          ()
    end

let scmd = 
  {(SubCommand.make
      ~std_usage:true
      "setup-clean"
      (s_ "Clean all template files from their content")
      CLIData.setup_clean_mkd
      main)
     with 
         scmd_specs =
           [
             "-replace-sections",
             Arg.Set replace_sections,
             s_ "Empty replace section in generated files (i.e. remove content \
                 between OASIS_START and OASIS_STOP).";
           ]}

let () = 
  SubCommand.register scmd

