open Json_encoding
open Matrix_common

module Get = struct
  module Query = Empty.Query

  module Response = struct
    module Tag = struct
      type t = {order: float option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.order in
        let of_tuple v =
          let order = v in
          {order} in
        let with_tuple = obj1 (opt "order" float) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {tags: (string * Tag.t) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.tags in
      let of_tuple v =
        let tags = v in
        {tags} in
      let with_tuple = obj1 (req "tags" (assoc Tag.encoding)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Put = struct
  module Query = Empty.Query

  module Request = struct
    type t = {order: float option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.order in
      let of_tuple v =
        let order = v in
        {order} in
      let with_tuple = obj1 (opt "order" float) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Delete = struct
  module Query = Empty.Query
  module Request = Empty.Json
  module Response = Empty.Json
end
