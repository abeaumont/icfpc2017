module type S = sig
  type t
  val make: Unix.file_descr -> Unix.file_descr -> t
  val send: t -> Online_t.message_punter_server -> unit
  val recv: t -> Online_t.message_server_punter
  val run: t -> unit
end

module Make(P: Punter.S) : S = struct
  type t = {
    ic: Unix.file_descr;
    oc: Unix.file_descr
  }

  let make ic oc = {ic; oc}

  let send t msg =
    let json = Online_j.string_of_message_punter_server msg in
    let json_length = String.length json in
    let prefix = string_of_int json_length in
    let len = json_length + String.length prefix + 1 in
    let buf = Buffer.create len in
    Buffer.add_string buf prefix;
    Buffer.add_char buf ':';
    Buffer.add_string buf json;
    Unix.write t.oc (Buffer.contents buf) 0 len |> ignore (* FIXME: Check result *)
    (* t.oc |> Unix.out_channel_of_descr |> flush *)

  let recv t =
    let read_size () =
      let rec loop size =
        let ch = input_char (Unix.in_channel_of_descr t.ic) in
        if ch = ':' then size
        else loop (size * 10 + (Char.code ch)  - 48) in
      loop 0 in
    let size = read_size () in
    let buf = Bytes.create size in
    Unix.read t.ic buf 0 size |> ignore; (* FIXME: Check result *)
    Online_j.message_server_punter_of_string buf

  let run t = ignore t (* TODO *)
end
