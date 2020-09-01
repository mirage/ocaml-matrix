open Json_encoding

module Put =
struct
  module Query = Empty.Query

  let path user_id data_type = "_matrix/client/r0/user/" ^ user_id ^ "/account_data/" ^ data_type

  module Request =
  struct
    type t =
      { data: Repr.value
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.data
      in
      let of_tuple v =
        let data = v in
        { data }
      in
      let with_tuple =
        any
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Get =
struct
  module Query = Empty.Query

  let path user_id data_type = "_matrix/client/r0/user/" ^ user_id ^ "/account_data/" ^ data_type

  module Response =
  struct
    type t =
      { data: (string * string) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.data
      in
      let of_tuple v =
        let data = v in
        { data }
      in
      let with_tuple =
        assoc string
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Put_by_room =
struct
  module Query = Empty.Query

  let path user_id room_id data_type = "_matrix/client/r0/user/" ^ user_id ^ "/rooms/" ^ room_id ^ "/account_data/" ^ data_type

  module Request =
  struct
    type t =
      { data: (string * string) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.data
      in
      let of_tuple v =
        let data = v in
        { data }
      in
      let with_tuple =
        assoc string
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Get_by_room =
struct
  module Query = Empty.Query

  let path user_id room_id data_type = "_matrix/client/r0/user/" ^ user_id ^ "/rooms/" ^ room_id ^ "/account_data/" ^ data_type

  module Response =
  struct
    type t =
      { data: (string * string) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.data
      in
      let of_tuple v =
        let data = v in
        { data }
      in
      let with_tuple =
        assoc string
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
