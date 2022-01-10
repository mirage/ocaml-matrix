open Json_encoding

module Response = struct
  type t = {server: string option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.server in
    let of_tuple v =
      let server = v in
      {server} in
    let with_tuple = obj1 (opt "m.server" string) in
    conv to_tuple of_tuple with_tuple
end
