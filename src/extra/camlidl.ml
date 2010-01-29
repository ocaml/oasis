
(** Camlidl package
  *)
module Camlidl =
struct 
  module Env = Environment
  module Chk = Check
  module Msg = Message

  let camlidl    = Chk.prog "camlidl"
end
