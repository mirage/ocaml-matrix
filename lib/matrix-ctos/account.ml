open Json_encoding
open Matrix_common

module Unbind_result = struct
  type t = Success | No_support

  let encoding = string_enum ["success", Success; "no-support", No_support]
end

module Email_request_token = struct
  module Query = Empty.Query

  module Request = struct
    type t = {
        client_secret: string
      ; email: string
      ; send_attempt: int
      ; next_link: string option
      ; id_server: string
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.client_secret, t.email, t.send_attempt, t.next_link, t.id_server in
      let of_tuple v =
        let client_secret, email, send_attempt, next_link, id_server = v in
        {client_secret; email; send_attempt; next_link; id_server} in
      let with_tuple =
        obj5
          (req "client_secret" string)
          (req "email" string) (req "send_attempt" int) (opt "next_link" string)
          (req "id_server" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {sid: string; submit_url: string option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.sid, t.submit_url in
      let of_tuple v =
        let sid, submit_url = v in
        {sid; submit_url} in
      let with_tuple = obj2 (req "sid" string) (opt "submit_url" string) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = false
end

module Msisdn_request_token = struct
  module Query = Empty.Query

  module Request = struct
    type t = {
        client_secret: string
      ; country: string
      ; phone_number: string
      ; send_attempt: int
      ; next_link: string option
      ; id_server: string
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.client_secret
        , t.country
        , t.phone_number
        , t.send_attempt
        , t.next_link
        , t.id_server ) in
      let of_tuple v =
        let ( client_secret
            , country
            , phone_number
            , send_attempt
            , next_link
            , id_server ) =
          v in
        {
          client_secret
        ; country
        ; phone_number
        ; send_attempt
        ; next_link
        ; id_server
        } in
      let with_tuple =
        obj6
          (req "client_secret" string)
          (req "country" string)
          (req "phone_number" string)
          (req "send_attempt" int) (opt "next_link" string)
          (req "id_server" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {sid: string; submit_url: string option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.sid, t.submit_url in
      let of_tuple v =
        let sid, submit_url = v in
        {sid; submit_url} in
      let with_tuple = obj2 (req "sid" string) (opt "submit_url" string) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = false
end

module Password = struct
  module Post = struct
    module Query = Empty.Query

    let path = "/_matrix/client/r0/account/password"

    module Request = struct
      type t = {auth: Authentication.Password.V1.t; new_password: string}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.auth, t.new_password in
        let of_tuple v =
          let auth, new_password = v in
          {auth; new_password} in
        let with_tuple =
          obj2
            (req "auth" Authentication.Password.V1.encoding)
            (req "new_password" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Response = Empty.Json

    let needs_auth = true
  end

  module Email_request_token = struct
    include Email_request_token

    let path = "/_matrix/client/r0/account/password/email/requestToken"
  end

  module Msisdn_request_token = struct
    include Msisdn_request_token

    let path = "/_matrix/client/r0/account/password/msisdn/requestToken"
  end
end

module Deactivate = struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/account/deactivate"

  module Request = struct
    type t = {auth: Authentication.Password.V1.t; id_server: string option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.auth, t.id_server in
      let of_tuple v =
        let auth, id_server = v in
        {auth; id_server} in
      let with_tuple =
        obj2
          (req "auth" Authentication.Password.V1.encoding)
          (opt "id_server" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {id_server_unbind_result: Unbind_result.t} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.id_server_unbind_result in
      let of_tuple v =
        let id_server_unbind_result = v in
        {id_server_unbind_result} in
      let with_tuple =
        obj1 (req "id_server_unbind_result" Unbind_result.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Third_party_id = struct
  module Medium = struct
    type t = Email | Msisdn

    let encoding = string_enum ["email", Email; "msisdn", Msisdn]
  end

  module Get = struct
    module Query = Empty.Query

    let path = "/_matrix/client/r0/account/3pid"

    module Response = struct
      module Third_party_identifier = struct
        type t = {
            medium: Medium.t
          ; address: string
          ; validated_at: int
          ; added_at: int
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.medium, t.address, t.validated_at, t.added_at in
          let of_tuple v =
            let medium, address, validated_at, added_at = v in
            {medium; address; validated_at; added_at} in
          let with_tuple =
            obj4
              (req "medium" Medium.encoding)
              (req "address" string) (req "validated_at" int)
              (req "added_at" int) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {threepids: Third_party_identifier.t list} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.threepids in
        let of_tuple v =
          let threepids = v in
          {threepids} in
        let with_tuple =
          obj1 (req "threepids" (list Third_party_identifier.encoding)) in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end

  module Post = struct
    module Query = Empty.Query

    let path = "/_matrix/client/r0/account/3pid"

    module Request = struct
      module Three_pid_creds = struct
        type t = {client_secret: string; id_server: string; sid: string}
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.client_secret, t.id_server, t.sid in
          let of_tuple v =
            let client_secret, id_server, sid = v in
            {client_secret; id_server; sid} in
          let with_tuple =
            obj3
              (req "client_secret" string)
              (req "id_server" string) (req "sid" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {three_pid_creds: Three_pid_creds.t; bind: bool option}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.three_pid_creds, t.bind in
        let of_tuple v =
          let three_pid_creds, bind = v in
          {three_pid_creds; bind} in
        let with_tuple =
          obj2
            (req "three_pid_creds" Three_pid_creds.encoding)
            (opt "bind" bool) in
        conv to_tuple of_tuple with_tuple
    end

    module Response = Empty.Json

    let needs_auth = true
  end

  module Delete = struct
    module Query = Empty.Query

    let path = "/_matrix/client/r0/account/3pid/delete"

    module Request = struct
      type t = {id_server: string option; medium: Medium.t; address: string}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.id_server, t.medium, t.address in
        let of_tuple v =
          let id_server, medium, address = v in
          {id_server; medium; address} in
        let with_tuple =
          obj3 (opt "id_server" string)
            (req "medium" Medium.encoding)
            (req "address" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Response = struct
      type t = {id_server_unbind_result: Unbind_result.t} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.id_server_unbind_result in
        let of_tuple v =
          let id_server_unbind_result = v in
          {id_server_unbind_result} in
        let with_tuple =
          obj1 (req "id_server_unbind_result" Unbind_result.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end

  module Email_request_token = struct
    include Email_request_token

    let path = "/_matrix/client/r0/account/3pid/email/requestToken"
  end

  module Msisdn_request_token = struct
    include Msisdn_request_token

    let path = "/_matrix/client/r0/account/3pid/msisdn/requestToken"
  end
end

module Whoami = struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/account/whoami"

  module Request = Empty.Json

  module Response = struct
    type t = {user_id: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.user_id in
      let of_tuple v =
        let user_id = v in
        {user_id} in
      let with_tuple = obj1 (req "user_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
