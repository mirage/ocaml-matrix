open Json_encoding

module Query =
struct
  type t =
    { limit: int option
    } [@@deriving accessor]

  let args t =
    match t.limit with
      | None -> []
      | Some limit ->
        let limit = Int.to_string limit in
        ["limit", [limit]]
end

let path room_id event_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/context/" ^ event_id

module Response =
struct
  type t =
    { start: string option
    ; end_: string option
    ; events_before: Room_events.t option
    ; event: string option
    ; events_after: string option
    ; state: Events.t list option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.start, t.end_, t.events_before, t.event, t.events_after, t.state
    in
    let of_tuple v =
      let start, end_, events_before, event, events_after, state = v in
      { start; end_; events_before; event; events_after; state }
    in
    let with_tuple =
      obj6
        (opt "start" string)
        (opt "end_" string)
        (opt "events_before" Room_events.encoding)
        (opt "event" string)
        (opt "events_after" string)
        (opt "state" (list Events.encoding))
    in
    conv to_tuple of_tuple with_tuple

end

let needs_auth = true
