open Json_encoding
open Matrix_common

module Server_key = struct
  module Verify_key = struct
    type t = {key: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.key in
      let of_tuple v =
        let key = v in
        {key} in
      let with_tuple = obj1 (req "key" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Old_verify_key = struct
    type t = {key: string; expired_ts: int} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.key, t.expired_ts in
      let of_tuple v =
        let key, expired_ts = v in
        {key; expired_ts} in
      let with_tuple = obj2 (req "key" string) (req "expired_ts" int) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
      server_name: string
    ; verify_keys: (string * Verify_key.t) list
    ; old_verify_keys: (string * Old_verify_key.t) list option
    ; signatures: string list option (* there is work to do here *)
    ; valid_until_ts: int option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.server_name
      , t.verify_keys
      , t.old_verify_keys
      , t.signatures
      , t.valid_until_ts ) in
    let of_tuple v =
      let server_name, verify_keys, old_verify_keys, signatures, valid_until_ts
          =
        v in
      {server_name; verify_keys; old_verify_keys; signatures; valid_until_ts}
    in
    let with_tuple =
      obj5 (req "server_name" string)
        (req "verify_keys" (assoc Verify_key.encoding))
        (opt "old_verify_keys" (assoc Old_verify_key.encoding))
        (opt "signatures" (list string))
        (opt "valid_until_ts" int) in
    conv to_tuple of_tuple with_tuple
end

module Direct_query = struct
  module Query = Empty.Query

  let path key_id = "/_matrix/key/v2/server/" ^ key_id
  (* The use of key id is deprecated, servers should ask for all the keys
     * instead
  *)

  module Response = Server_key
end

module Indirect_query = struct
  module Query = struct
    type t = {minimum_valid_until_ts: int option} [@@deriving accessor]

    let args t =
      match t.minimum_valid_until_ts with
      | None -> []
      | Some minimum_valid_until_ts ->
        let minimum_valid_until_ts = Int.to_string minimum_valid_until_ts in
        ["minimum_valid_until_ts", [minimum_valid_until_ts]]
  end

  let path server_name key_id =
    "/_matrix/key/v2/query/" ^ server_name ^ "/" ^ key_id
  (* The use of key id is deprecated, servers should ask for all the keys
     * instead
  *)

  module Response = struct
    type t = {server_keys: Server_key.t list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.server_keys in
      let of_tuple v =
        let server_keys = v in
        {server_keys} in
      let with_tuple = obj1 (req "server_keys" (list Server_key.encoding)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Indirect_batch_query = struct
  module Query = Empty.Query

  let path = "/_matrix/key/v2/query"

  module Request = struct
    module Query_criteria = struct
      type t = {minimum_valid_until_ts: int option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.minimum_valid_until_ts in
        let of_tuple v =
          let minimum_valid_until_ts = v in
          {minimum_valid_until_ts} in
        let with_tuple = obj1 (opt "minimum_valid_until_ts" int) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {server_keys: (string * (string * Query_criteria.t) list) list}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.server_keys in
      let of_tuple v =
        let server_keys = v in
        {server_keys} in
      let with_tuple =
        obj1 (req "server_keys" (assoc (assoc Query_criteria.encoding))) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {server_keys: Server_key.t list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.server_keys in
      let of_tuple v =
        let server_keys = v in
        {server_keys} in
      let with_tuple = obj1 (req "server_keys" (list Server_key.encoding)) in
      conv to_tuple of_tuple with_tuple
  end
end
