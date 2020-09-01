open Json_encoding

module Post =
struct
  module Query = Empty.Query

  let path user_id = "_matrix/client/r0/presence/" ^ user_id ^ "/status"

  module Request =
  struct
    type t =
      { presence: Event.Presence.Presence.t
      ; status_msg: string option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.presence, t.status_msg
      in
      let of_tuple v =
        let presence, status_msg = v in
        { presence; status_msg }
      in
      let with_tuple =
        obj2
          (req "presence" Event.Presence.Presence.encoding)
          (opt "status_msg" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Get =
struct
  module Query = Empty.Query

  let path user_id = "_matrix/client/r0/presence/" ^ user_id ^ "/status"

  module Response =
  struct
    type t =
      { presence: Event.Presence.Presence.t
      ; last_active_ago: int option
      ; status_msg: string option
      ; currently_active: bool option
      ; user_id: string option (* Not indicated in the documentation, but in the response os synapse *)
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.presence, t.last_active_ago, t.status_msg, t.currently_active, t.user_id
      in
      let of_tuple v =
        let presence, last_active_ago, status_msg, currently_active, user_id = v in
        { presence; last_active_ago; status_msg; currently_active; user_id }
      in
      let with_tuple =
        obj5
          (req "presence" Event.Presence.Presence.encoding)
          (opt "last_active_ago" int)
          (opt "status_msg" string)
          (opt "currently_active" bool)
          (opt "user_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
