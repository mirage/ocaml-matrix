open Json_encoding

type t = {
    algorithm: string
  ; forwarding_curve25519_key_chain: string list
  ; room_id: string
  ; sender_key: string
  ; sender_claimed_keys: (string * string) list
  ; session_id: string
  ; session_key: string
}
[@@deriving accessor]

let encoding =
  let to_tuple t =
    ( t.algorithm
    , t.forwarding_curve25519_key_chain
    , t.room_id
    , t.sender_key
    , t.sender_claimed_keys
    , t.session_id
    , t.session_key ) in
  let of_tuple v =
    let ( algorithm
        , forwarding_curve25519_key_chain
        , room_id
        , sender_key
        , sender_claimed_keys
        , session_id
        , session_key ) =
      v in
    {
      algorithm
    ; forwarding_curve25519_key_chain
    ; room_id
    ; sender_key
    ; sender_claimed_keys
    ; session_id
    ; session_key
    } in
  let with_tuple =
    obj7 (req "algorithm" string)
      (req "forwarding_curve25519_key_chain" (list string))
      (req "room_id" string) (req "sender_key" string)
      (req "sender_claimed_keys" (assoc string))
      (req "session_id" string) (req "session_key" string) in
  conv to_tuple of_tuple with_tuple
