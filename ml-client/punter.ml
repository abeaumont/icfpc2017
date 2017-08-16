open Online_t

module type S = sig
  type t
  val make: setup_server_punter -> t
  val turn: t -> move
  val stop: t -> unit
end

type t = setup_server_punter

let make msg = msg
let turn t = `Pass {punter = t.punter}
let stop _ = ()

