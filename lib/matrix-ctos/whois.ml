open Json_encoding
open Matrix_common
module Query = Empty.Query

let path user_id = "_matrix/client/r0/admin/whois/" ^ user_id

module Response = struct
  module Device_info = struct
    module Session_info = struct
      module Connection_info = struct
        type t = {
            ip: string option
          ; last_seen: int option
          ; user_agent: string option
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.ip, t.last_seen, t.user_agent in
          let of_tuple v =
            let ip, last_seen, user_agent = v in
            {ip; last_seen; user_agent} in
          let with_tuple =
            obj3 (opt "ip" string) (opt "last_seen" int)
              (opt "user_agent" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {connections: Connection_info.t list option}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.connections in
        let of_tuple v =
          let connections = v in
          {connections} in
        let with_tuple =
          obj1 (opt "connections" (list Connection_info.encoding)) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {sessions: Session_info.t list option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.sessions in
      let of_tuple v =
        let sessions = v in
        {sessions} in
      let with_tuple = obj1 (opt "sessions" (list Session_info.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
      user_id: string option
    ; devices: (string * Device_info.t) list option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.user_id, t.devices in
    let of_tuple v =
      let user_id, devices = v in
      {user_id; devices} in
    let with_tuple =
      obj2 (opt "user_id" string) (opt "devices" (assoc Device_info.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
