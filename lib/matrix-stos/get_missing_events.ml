open Json_encoding
open Matrix_common
module Query = Empty.Query

module Request = struct
  type t = {
    limit: int option;
    min_depth: int option;
    earliest_events: string list;
    lastest_events: string list;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.limit, t.min_depth, t.earliest_events, t.lastest_events in
    let of_tuple v =
      let limit, min_depth, earliest_events, lastest_events = v in
      {limit; min_depth; earliest_events; lastest_events} in
    let with_tuple =
      obj4 (opt "limit" int) (opt "min_depth" int)
        (req "earliest_events" (list string))
        (req "lastest_events" (list string)) in
    conv to_tuple of_tuple with_tuple
end

module Response = struct
  type t = {events: Pdu.t list} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.events in
    let of_tuple v =
      let events = v in
      {events} in
    let with_tuple = obj1 (req "origin" (list Pdu.encoding)) in
    conv to_tuple of_tuple with_tuple
end
