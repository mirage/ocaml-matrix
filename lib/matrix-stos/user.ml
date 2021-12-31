open Json_encoding

module Devices = struct
  module Response = struct
    module User_device = struct
      module Device_keys = struct
        type t = {
          algorithms: string list;
          device_id: string;
          keys: (string * string) list;
          signatures: (string * (string * string) list) list;
          user_id: string;
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t =
            t.algorithms, t.device_id, t.keys, t.signatures, t.user_id in
          let of_tuple v =
            let algorithms, device_id, keys, signatures, user_id = v in
            {algorithms; device_id; keys; signatures; user_id} in
          let with_tuple =
            obj5
              (req "algorithms" (list string))
              (req "device_id" string)
              (req "keys" (assoc string))
              (req "signatures" (assoc (assoc string)))
              (req "user_id" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {
        device_display_name: string option;
        device_id: string;
        keys: Device_keys.t;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.device_display_name, t.device_id, t.keys in
        let of_tuple v =
          let device_display_name, device_id, keys = v in
          {device_display_name; device_id; keys} in
        let with_tuple =
          obj3
            (opt "device_display_name" string)
            (req "device_id" string)
            (req "keys" Device_keys.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Cross_signing_key = struct
      type t = {
        keys: (string * string) list;
        signatures: (string * (string * string) list) list;
        usage: string list;
        user_id: string;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.keys, t.signatures, t.usage, t.user_id in
        let of_tuple v =
          let keys, signatures, usage, user_id = v in
          {keys; signatures; usage; user_id} in
        let with_tuple =
          obj4
            (req "keys" (assoc string))
            (req "signatures" (assoc (assoc string)))
            (req "usage" (list string))
            (req "user_id" string) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      devices: User_device.t list;
      master_key: Cross_signing_key.t option;
      self_signing_key: Cross_signing_key.t option;
      stream_id: int;
      user_id: string;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.devices, t.master_key, t.self_signing_key, t.stream_id, t.user_id
      in
      let of_tuple v =
        let devices, master_key, self_signing_key, stream_id, user_id = v in
        {devices; master_key; self_signing_key; stream_id; user_id} in
      let with_tuple =
        obj5
          (req "devices" (list User_device.encoding))
          (opt "master_key" Cross_signing_key.encoding)
          (opt "self_signing_key" Cross_signing_key.encoding)
          (req "stream_id" int) (req "user_id" string) in
      conv to_tuple of_tuple with_tuple
  end
end
