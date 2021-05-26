open Json_encoding
open Matrix_common

module Upload = struct
  module Query = Empty.Query

  module Request = struct
    module Device_keys = struct
      type t = {
          user_id: string
        ; device_id: string
        ; algorithms: string list
        ; keys: (string * string) list
        ; signatures: (string * (string * string) list) list
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.user_id, t.device_id, t.algorithms, t.keys, t.signatures in
        let of_tuple v =
          let user_id, device_id, algorithms, keys, signatures = v in
          {user_id; device_id; algorithms; keys; signatures} in
        let with_tuple =
          obj5 (req "user_id" string) (req "device_id" string)
            (req "algorithms" (list string))
            (req "keys" (assoc string))
            (req "signatures" (assoc (assoc string))) in
        conv to_tuple of_tuple with_tuple
    end

    module Keys_format = struct
      type t = Key of string | Object_key of Ezjsonm.value

      let encoding =
        union
          [
            case string (function Key t -> Some t | _ -> None) (fun t -> Key t)
          ; case any
              (function Object_key t -> Some t | _ -> None)
              (fun t -> Object_key t)
          ]
    end

    type t = {
        device_keys: Device_keys.t option
      ; one_time_keys: (string * Keys_format.t) list option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.device_keys, t.one_time_keys in
      let of_tuple v =
        let device_keys, one_time_keys = v in
        {device_keys; one_time_keys} in
      let with_tuple =
        obj2
          (opt "device_keys" Device_keys.encoding)
          (opt "one_time_keys" (assoc Keys_format.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {one_time_key_counts: (string * int) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.one_time_key_counts in
      let of_tuple v =
        let one_time_key_counts = v in
        {one_time_key_counts} in
      let with_tuple = obj1 (req "one_time_key_counts" (assoc int)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Query = struct
  module Query = Empty.Query

  module Request = struct
    type t = {
        timeout: int option
      ; device_keys: (string * string list) list
      ; token: string option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.timeout, t.device_keys, t.token in
      let of_tuple v =
        let timeout, device_keys, token = v in
        {timeout; device_keys; token} in
      let with_tuple =
        obj3 (opt "timeout" int)
          (req "device_keys" (assoc (list string)))
          (opt "token" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    module Device_keys = struct
      module Unsigned_device_info = struct
        type t = {device_display_name: string option} [@@deriving accessor]

        let encoding =
          let to_tuple t = t.device_display_name in
          let of_tuple v =
            let device_display_name = v in
            {device_display_name} in
          let with_tuple = obj1 (opt "device_display_name" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {
          user_id: string
        ; device_id: string
        ; algorithms: string list
        ; keys: (string * string) list
        ; signatures: (string * (string * string) list) list
        ; unsigned: Unsigned_device_info.t option
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.user_id, t.device_id, t.algorithms, t.keys, t.signatures, t.unsigned
        in
        let of_tuple v =
          let user_id, device_id, algorithms, keys, signatures, unsigned = v in
          {user_id; device_id; algorithms; keys; signatures; unsigned} in
        let with_tuple =
          obj6 (req "user_id" string) (req "device_id" string)
            (req "algorithms" (list string))
            (req "keys" (assoc string))
            (req "signatures" (assoc (assoc string)))
            (opt "unsigned" Unsigned_device_info.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
        failures: (string * Ezjsonm.value) list option
      ; device_keys: (string * (string * Device_keys.t) list) list option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.failures, t.device_keys in
      let of_tuple v =
        let failures, device_keys = v in
        {failures; device_keys} in
      let with_tuple =
        obj2
          (opt "failures" (assoc any))
          (opt "device_keys" (assoc (assoc Device_keys.encoding))) in
      conv to_tuple of_tuple with_tuple
  end
end

module Claim = struct
  module Query = Empty.Query

  module Request = struct
    type t = {
        timeout: int
      ; one_time_keys: (string * (string * string) list) list
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.timeout, t.one_time_keys in
      let of_tuple v =
        let timeout, one_time_keys = v in
        {timeout; one_time_keys} in
      let with_tuple =
        obj2 (req "timeout" int) (req "one_time_keys" (assoc (assoc string)))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {
        failures: (string * Ezjsonm.value) list
      ; one_time_keys: (string * (string * string) list) list (* to correct *)
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.failures, t.one_time_keys in
      let of_tuple v =
        let failures, one_time_keys = v in
        {failures; one_time_keys} in
      let with_tuple =
        obj2
          (req "failures" (assoc any))
          (req "one_time_keys" (assoc (assoc string))) in
      conv to_tuple of_tuple with_tuple
  end
end

module Changes = struct
  module Query = struct
    type t = {from: string; _to: string} [@@deriving accessor]

    let args t = ["from", [t.from]; "to", [t._to]]
  end

  module Response = struct
    type t = {changed: string list option; left: string list option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.changed, t.left in
      let of_tuple v =
        let changed, left = v in
        {changed; left} in
      let with_tuple =
        obj2 (opt "changed" (list string)) (opt "left" (list string)) in
      conv to_tuple of_tuple with_tuple
  end
end
