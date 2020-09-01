open Json_encoding

module Query = Empty.Query

let path room_id event_id = "/_matrix/federation/v1/event_auth/" ^ room_id ^ "/" ^ event_id

module Response =
struct
  type t =
    { auth_chain: Matrix_ctos.Events.t list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.auth_chain
    in
    let of_tuple v =
      let auth_chain = v in
      { auth_chain }
    in
    let with_tuple =
      obj1
        (req "auth_chain" (list Matrix_ctos.Events.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
