open Json_encoding
open Matrix_common

module Query = struct
  type t = {from: string option; timeout: int option; room_id: string option}
  [@@deriving accessor]

  let args t =
    let l = [] in
    let l = match t.from with None -> l | Some from -> ("from", [from]) :: l in
    let l =
      match t.timeout with
      | None -> l
      | Some timeout ->
        let timeout = Int.to_string timeout in
        ("timeout", [timeout]) :: l in
    let l =
      match t.room_id with
      | None -> l
      | Some room_id -> ("room_id", [room_id]) :: l in
    l
end

module Response = struct
  type t = {
    start: string option;
    end_: string option;
    chunk: Events.Room_event.t list option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.start, t.end_, t.chunk in
    let of_tuple v =
      let start, end_, chunk = v in
      {start; end_; chunk} in
    let with_tuple =
      obj3 (opt "start" string) (opt "end" string)
        (opt "chunk" (list Events.Room_event.encoding)) in
    conv to_tuple of_tuple with_tuple
end
