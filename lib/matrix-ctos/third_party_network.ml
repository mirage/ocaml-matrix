open Json_encoding
open Matrix_common

module Protocol =
struct
  module Field_type =
  struct
    type t =
      { regexp: string
      ; placeholder: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.regexp, t.placeholder
      in
      let of_tuple v =
        let regexp, placeholder = v in
        { regexp; placeholder }
      in
      let with_tuple =
        obj2
          (req "regexp" string)
          (req "placeholder" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Instance =
  struct
    type t =
      { desc: string
      ; icon: string option
      ; fields: (string * string) list
      ; network_id: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.desc, t.icon, t.fields, t.network_id
      in
      let of_tuple v =
        let desc, icon, fields, network_id = v in
        { desc; icon; fields; network_id }
      in
      let with_tuple =
        obj4
          (req "desc" string)
          (opt "icon" string)
          (req "fields" (assoc string))
          (req "network_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    { user_fields: string list
    ; location_fields: string list
    ; icon: string
    ; field_types: (string * Field_type.t) list
    ; instances: Instance.t list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.user_fields, t.location_fields, t.icon, t.field_types, t.instances
    in
    let of_tuple v =
      let user_fields, location_fields, icon, field_types, instances = v in
      { user_fields; location_fields; icon; field_types; instances }
    in
    let with_tuple =
      obj5
        (req "user_fields" (list string))
        (req "location_fields" (list string))
        (req "icon" string)
        (req "field_types" (assoc Field_type.encoding))
        (req "instances" (list Instance.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

module Location =
struct
  type t =
    { alias: string
    ; protocol: string
    ; fields: (string * string) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.alias, t.protocol, t.fields
    in
    let of_tuple v =
      let alias, protocol, fields = v in
      { alias; protocol; fields }
    in
    let with_tuple =
      obj3
        (req "alias" string)
        (req "protocol" string)
        (req "fields" (assoc string))
    in
    conv to_tuple of_tuple with_tuple
end

module User =
struct
  type t =
    { user_id: string
    ; protocol: string
    ; fields: (string * string) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.user_id, t.protocol, t.fields
    in
    let of_tuple v =
      let user_id, protocol, fields = v in
      { user_id; protocol; fields }
    in
    let with_tuple =
      obj3
        (req "userid" string)
        (req "protocol" string)
        (req "fields" (assoc string))
    in
    conv to_tuple of_tuple with_tuple
end

module Protocols =
struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/thirdparty/protocols"

  module Response =
  struct
    type t =
      { protocols: (string * Protocol.t) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.protocols
      in
      let of_tuple v =
        let protocols = v in
        { protocols }
      in
      let with_tuple =
        assoc Protocol.encoding
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Get_protocol =
struct
  module Query = Empty.Query

  let path protocol = "_matrix/client/r0/thirdparty/protocol/" ^ protocol

  module Response = Protocol

  let needs_auth = true
end

module Get_location =
struct
  module Query =
  struct
    type t =
      { search_fields: string option
      } [@@deriving accessor]

    let args t =
      match t.search_fields with
        | None -> []
        | Some search_fields -> ["searchFields", [search_fields]]
  end

  let path protocol = "_matrix/client/r0/thirdparty/location/" ^ protocol

  module Response =
  struct
    type t =
      { locations: Location.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.locations
      in
      let of_tuple v =
        let locations = v in
        { locations }
      in
      let with_tuple =
        list Location.encoding
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Get_user =
struct
  module Query =
  struct
    type t =
      { fields: (string * string list) list option
      } [@@deriving accessor]

    let args t =
      match t.fields with
        | None -> []
        | Some fields -> fields
  end

  let path protocol = "_matrix/client/r0/thirdparty/user/" ^ protocol

  module Response =
  struct
    type t =
      { users: User.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.users
      in
      let of_tuple v =
        let users = v in
        { users }
      in
      let with_tuple =
        list User.encoding
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Location_from_alias =
struct
  module Query =
  struct
    type t =
      { alias: string
      } [@@deriving accessor]

    let args t =
      ["alias", [t.alias]]
  end

  let path = "/_matrix/client/r0/thirdparty/location"

  module Response =
  struct
    type t =
      { locations: Location.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.locations
      in
      let of_tuple v =
        let locations = v in
        { locations }
      in
      let with_tuple =
        list Location.encoding
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module User_from_user_id =
struct
  module Query =
  struct
    type t =
      { user_id: string
      } [@@deriving accessor]

    let args t =
      ["userid", [t.user_id]]
  end

  let path = "/_matrix/client/r0/thirdparty/user"

  module Response =
  struct
    type t =
      { users: User.t list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.users
      in
      let of_tuple v =
        let users = v in
        { users }
      in
      let with_tuple =
        list User.encoding
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
