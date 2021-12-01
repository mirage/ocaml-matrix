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
