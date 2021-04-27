open Json_encoding

module Query = struct
  type t = {v: string list; limit: int} [@@deriving accessor]

  let args t =
    let l = ["v", t.v] in
    let l =
      let limit = Int.to_string t.limit in
      ("limit", [limit]) :: l in
    l
end

let path room_id = "/_matrix/federation/v1/backfill/" ^ room_id

module Response = struct
  type t = {origin: string; origin_server_ts: int; pdus: Pdu.t list}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.origin, t.origin_server_ts, t.pdus in
    let of_tuple v =
      let origin, origin_server_ts, pdus = v in
      {origin; origin_server_ts; pdus} in
    let with_tuple =
      obj3 (req "origin" string)
        (req "origin_server_ts" int)
        (req "pdus" (list Pdu.encoding)) in
    conv to_tuple of_tuple with_tuple
end
