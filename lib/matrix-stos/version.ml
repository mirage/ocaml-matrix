open Json_encoding
open Matrix_common
module Query = Empty.Query

module Response = struct
  type t = {name: string option; version: string option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.name, t.version in
    let of_tuple v =
      let name, version = v in
      {name; version} in
    let with_tuple =
      obj1
        (req "server"
           (obj2 (opt "versions" string) (opt "unstable_features" string)))
    in
    conv to_tuple of_tuple with_tuple
end
