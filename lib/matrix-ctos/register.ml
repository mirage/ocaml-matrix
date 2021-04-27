open Json_encoding
open Matrix_common

module Register = struct
  module Query = struct
    module Kind = struct
      type t = User | Guest

      let to_string = function User -> "user" | Guest -> "guest"
    end

    type t = {kind: Kind.t option} [@@deriving accessor]

    let args t =
      match t.kind with
      | None -> []
      | Some kind -> ["kind", [Kind.to_string kind]]
  end

  module Request = struct
    type t = {
        auth: Authentication.t option
      ; bind_email: bool option
      ; bind_msisdn: bool option
      ; username: string option
      ; password: string option
      ; device_id: string option
      ; initial_device_display_name: string option
      ; inhibit_login: bool option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.auth
        , t.bind_email
        , t.bind_msisdn
        , t.username
        , t.password
        , t.device_id
        , t.initial_device_display_name
        , t.inhibit_login
        , None ) in
      let of_tuple v =
        let ( auth
            , bind_email
            , bind_msisdn
            , username
            , password
            , device_id
            , initial_device_display_name
            , inhibit_login
            , _ ) =
          v in
        {
          auth
        ; bind_email
        ; bind_msisdn
        ; username
        ; password
        ; device_id
        ; initial_device_display_name
        ; inhibit_login
        } in
      let with_tuple =
        obj9
          (opt "auth" Authentication.encoding)
          (opt "bind_email" bool) (opt "bind_msisdn" bool)
          (opt "username" string) (opt "password" string)
          (opt "device_id" string)
          (opt "initial_device_display_name" string)
          (opt "inhibit_login" bool) (opt "x_show_msisdn" unit)
        (* Not in documentation *) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {
        user_id: string
      ; access_token: string option
      ; home_server: string
      ; device_id: string option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.user_id, t.access_token, t.home_server, t.device_id in
      let of_tuple v =
        let user_id, access_token, home_server, device_id = v in
        {user_id; access_token; home_server; device_id} in
      let with_tuple =
        obj4 (req "user_id" string)
          (opt "access_token" string)
          (req "home_server" string) (opt "device_id" string) in
      conv to_tuple of_tuple with_tuple
  end
end

module Available = struct
  module Query = struct
    type t = {username: string} [@@deriving accessor]

    let args t = ["username", [t.username]]
  end

  module Request = Empty.Json

  module Response = struct
    type t = {available: bool} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.available in
      let of_tuple v =
        let available = v in
        {available} in
      let with_tuple = obj1 (req "available" bool) in
      conv to_tuple of_tuple with_tuple
  end
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
          (req "email" string) (req "send_attempt" int) (opt "device_id" string)
          (req "id_server" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {success: bool option; sid: string; submit_url: string option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.success, t.sid, t.submit_url in
      let of_tuple v =
        let success, sid, submit_url = v in
        {success; sid; submit_url} in
      let with_tuple =
        obj3 (opt "success" bool) (req "sid" string) (opt "submit_url" string)
      in
      conv to_tuple of_tuple with_tuple
  end
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
          (req "send_attempt" int) (opt "device_id" string)
          (req "id_server" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = struct
    type t = {
        success: bool option
      ; sid: string
      ; submit_url: string option
      ; msisdn: string option
      ; intl_fmt: string option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.success, t.sid, t.submit_url, t.msisdn, t.intl_fmt in
      let of_tuple v =
        let success, sid, submit_url, msisdn, intl_fmt = v in
        {success; sid; submit_url; msisdn; intl_fmt} in
      let with_tuple =
        obj5 (opt "success" bool) (req "sid" string) (opt "submit_url" string)
          (opt "msisdn" string) (opt "intl_fmt" string) in
      conv to_tuple of_tuple with_tuple
  end
end
