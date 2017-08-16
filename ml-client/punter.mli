type t

module type S = sig
  val make: Online_t.setup_server_punter -> t
  val turn: t -> Online_t.move
  val stop: t -> unit
end
