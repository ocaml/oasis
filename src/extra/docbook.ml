
(** Docbook package
  *)
module Docbook =
struct
  module Env = Environment
  module Chk = Check
  module Msg = Message

  let xsltproc   = Chk.prog "xsltproc"
  let xmllint    = Chk.prog "xmllint"

end
;;


