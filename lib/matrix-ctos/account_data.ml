open Json_encoding
open Matrix_common

module Put = struct
  module Query = Empty.Query

  module Request = struct
    type t = {data: Ezjsonm.value} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.data in
      let of_tuple v =
        let data = v in
        {data} in
      let with_tuple = any in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Get = struct
  module Query = Empty.Query

  module Response = struct
    type t = {data: Ezjsonm.value} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.data in
      let of_tuple v =
        let data = v in
        {data} in
      let with_tuple = any in
      conv to_tuple of_tuple with_tuple
  end
end

module Put_by_room = struct
  module Query = Empty.Query

  module Request = struct
    type t = {data: (string * string) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.data in
      let of_tuple v =
        let data = v in
        {data} in
      let with_tuple = assoc string in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Get_by_room = struct
  module Query = Empty.Query

  module Response = struct
    type t = {data: (string * string) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.data in
      let of_tuple v =
        let data = v in
        {data} in
      let with_tuple = assoc string in
      conv to_tuple of_tuple with_tuple
  end
end
