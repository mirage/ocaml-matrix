open Json_encoding
open Matrix_common

module Query = Empty.Query

let path txn_id = "/_matrix/federation/v1/send/" ^ txn_id

module Request =
struct
  type t =
    { origin: string
    ; origin_server_ts: int
    ; pdus: Pdu.t list
    ; edus: Edu.t list option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.origin, t.origin_server_ts, t.pdus, t.edus
    in
    let of_tuple v =
      let origin, origin_server_ts, pdus, edus = v in
      { origin; origin_server_ts; pdus; edus }
    in
    let with_tuple =
      obj4
        (req "origin" string)
        (req "origin_server_ts" int)
        (req "pdus" (list Pdu.encoding))
        (opt "edus" (list Edu.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

module Response =
struct
  module Pdu_processing_result =
  struct
    type t =
      { error: string option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.error
      in
      let of_tuple v =
        let error = v in
        { error }
      in
      let with_tuple =
        obj1
          (opt "error" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    { pdus: (string * Pdu_processing_result.t) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.pdus
    in
    let of_tuple v =
      let pdus = v in
      { pdus }
    in
    let with_tuple =
      obj1
        (req "pdus" (assoc Pdu_processing_result.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
