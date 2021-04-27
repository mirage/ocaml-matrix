open Json_encoding

module Verification = struct
  module Request = struct
    type t = {
        from_device: string
      ; transaction_id: string
      ; methods: string list
      ; timestamp: int
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.from_device, t.transaction_id, t.methods, t.timestamp in
      let of_tuple v =
        let from_device, transaction_id, methods, timestamp = v in
        {from_device; transaction_id; methods; timestamp} in
      let with_tuple =
        obj4 (req "from_device" string)
          (req "transaction_id" string)
          (req "methods" (list string))
          (req "timestamp" int) in
      conv to_tuple of_tuple with_tuple
  end

  module Start = struct
    type t = {
        from_device: string
      ; transaction_id: string
      ; verification_method: string
      ; next_method: string
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.from_device, t.transaction_id, t.verification_method, t.next_method
      in
      let of_tuple v =
        let from_device, transaction_id, verification_method, next_method = v in
        {from_device; transaction_id; verification_method; next_method} in
      let with_tuple =
        obj4 (req "from_device" string)
          (req "transaction_id" string)
          (req "method" string) (req "timestamp" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Cancel = struct
    type t = {transaction_id: string; reason: string; code: string}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.transaction_id, t.reason, t.code in
      let of_tuple v =
        let transaction_id, reason, code = v in
        {transaction_id; reason; code} in
      let with_tuple =
        obj3
          (req "transaction_id" string)
          (req "reason" string) (req "code" string) in
      conv to_tuple of_tuple with_tuple
  end
end

module Sas_verification = struct
  module Sas = struct
    type t = Decimal | Emoji

    let encoding = string_enum ["decimal", Decimal; "emoji", Emoji]
  end

  module Start = struct
    type t = {
        from_device: string
      ; transaction_id: string
      ; verification_method: string
      ; key_agreement_protocols: string list
      ; hashes: string list
      ; message_authentication_codes: string list
      ; short_authentication_string: Sas.t list
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.from_device
        , t.transaction_id
        , t.verification_method
        , t.key_agreement_protocols
        , t.hashes
        , t.message_authentication_codes
        , t.short_authentication_string ) in
      let of_tuple v =
        let ( from_device
            , transaction_id
            , verification_method
            , key_agreement_protocols
            , hashes
            , message_authentication_codes
            , short_authentication_string ) =
          v in
        {
          from_device
        ; transaction_id
        ; verification_method
        ; key_agreement_protocols
        ; hashes
        ; message_authentication_codes
        ; short_authentication_string
        } in
      let with_tuple =
        obj7 (req "from_device" string)
          (req "transaction_id" string)
          (req "verification_method" string)
          (req "key_agreement_protocols" (list string))
          (req "hashes" (list string))
          (req "message_authentication_codes" (list string))
          (req "short_authentication_string" (list Sas.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  module Accept = struct
    type t = {
        transaction_id: string
      ; verification_method: string
      ; key_agreement_protocol: string
      ; hash: string
      ; message_authentication_code: string
      ; short_authentication_string: Sas.t list
      ; commitment: string
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.transaction_id
        , t.verification_method
        , t.key_agreement_protocol
        , t.hash
        , t.message_authentication_code
        , t.short_authentication_string
        , t.commitment ) in
      let of_tuple v =
        let ( transaction_id
            , verification_method
            , key_agreement_protocol
            , hash
            , message_authentication_code
            , short_authentication_string
            , commitment ) =
          v in
        {
          transaction_id
        ; verification_method
        ; key_agreement_protocol
        ; hash
        ; message_authentication_code
        ; short_authentication_string
        ; commitment
        } in
      let with_tuple =
        obj7
          (req "transaction_id" string)
          (req "verification_method" string)
          (req "key_agreement_protocol" string)
          (req "hash" string)
          (req "message_authentication_code" string)
          (req "short_authentication_string" (list Sas.encoding))
          (req "commitment" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Key = struct
    type t = {transaction_id: string; key: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.transaction_id, t.key in
      let of_tuple v =
        let transaction_id, key = v in
        {transaction_id; key} in
      let with_tuple = obj2 (req "transaction_id" string) (req "key" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Mac = struct
    type t = {transaction_id: string; mac: (string * string) list; keys: string}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.transaction_id, t.mac, t.keys in
      let of_tuple v =
        let transaction_id, mac, keys = v in
        {transaction_id; mac; keys} in
      let with_tuple =
        obj3
          (req "transaction_id" string)
          (req "mac" (assoc string))
          (req "keys" string) in
      conv to_tuple of_tuple with_tuple
  end
end
