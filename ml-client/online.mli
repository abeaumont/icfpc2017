module type S = sig
  type t
  val make: Unix.file_descr -> Unix.file_descr -> t
  val send: t -> Online_t.message_punter_server -> unit
  val recv: t -> Online_t.message_server_punter
  val run: t -> unit
end

module Make (P: Punter.S) : S
