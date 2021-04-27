open Json_encoding
open Matrix_common

module Query = struct
  type t = {limit: int option} [@@deriving accessor]

  let args t =
    match t.limit with
    | None -> []
    | Some limit ->
      let limit = Int.to_string limit in
      ["limit", [limit]]
end

module Response = struct
  type t = {
      start: string option
    ; end_: string option
    ; events_before: Events.Room_event.t list option
    ; event: Events.Room_event.t option
    ; events_after: Events.Room_event.t list option
    ; state: Events.State_event.t list option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.start, t.end_, t.events_before, t.event, t.events_after, t.state in
    let of_tuple v =
      let start, end_, events_before, event, events_after, state = v in
      {start; end_; events_before; event; events_after; state} in
    let with_tuple =
      obj6 (opt "start" string) (opt "end_" string)
        (opt "events_before" (list Events.Room_event.encoding))
        (opt "event" Events.Room_event.encoding)
        (opt "events_after" (list Events.Room_event.encoding))
        (opt "state" (list Events.State_event.encoding)) in
    conv to_tuple of_tuple with_tuple
end
