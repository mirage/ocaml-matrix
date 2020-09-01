open Json_encoding

module State =
struct
  module Query =
  struct
    type t =
      { event_id: string
      } [@@deriving accessor]

    let args t =
      ["event_id", [t.event_id]]
  end

  let path room_id = "/_matrix/federation/v1/state/" ^ room_id

  module Response =
  struct
    type t =
      { auth_chain: Matrix_ctos.Events.t list
      ; pdus: Matrix_ctos.Events.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.auth_chain, t.pdus
      in
      let of_tuple v =
        let auth_chain, pdus = v in
        { auth_chain; pdus }
      in
      let with_tuple =
        obj2
          (req "auth_chain" (list Matrix_ctos.Events.encoding))
          (req "pdus" (list Matrix_ctos.Events.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module State_ids =
struct
  module Query =
  struct
    type t =
      { event_id: string
      } [@@deriving accessor]

    let args t =
      ["event_id", [t.event_id]]
  end

  let path room_id = "/_matrix/federation/v1/state/" ^ room_id

  module Response =
  struct
    type t =
      { auth_chain_ids: string list
      ; pdus_ids: string list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.auth_chain_ids, t.pdus_ids
      in
      let of_tuple v =
        let auth_chain_ids, pdus_ids = v in
        { auth_chain_ids; pdus_ids }
      in
      let with_tuple =
        obj2
          (req "auth_chain_ids" (list string))
          (req "pdus_ids" (list string))
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Event =
struct
  module Query = Empty.Query

  let path event_id = "/_matrix/federation/v1/event/" ^ event_id

  module Response =
  struct
    type t =
      { origin: string
      ; origin_server_ts: int
      ; pdus: Pdu.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.origin, t.origin_server_ts, t.pdus
      in
      let of_tuple v =
        let origin, origin_server_ts, pdus = v in
        { origin; origin_server_ts; pdus }
      in
      let with_tuple =
        obj3
          (req "origin" string)
          (req "origin_server_ts" int)
          (req "pdus" (list Pdu.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
