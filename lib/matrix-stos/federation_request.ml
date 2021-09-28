open Json_encoding

module Obj = struct
  type 'a t = {
    meth: string;
    uri: string;
    origin: string;
    destination: string option;
    content: 'a option;
  }
  [@@deriving accessor]

  let encoding encoding =
    let to_tuple t = t.meth, t.uri, t.origin, t.destination, t.content in
    let of_tuple v =
      let meth, uri, origin, destination, content = v in
      {meth; uri; origin; destination; content} in
    let with_tuple =
      obj5 (req "method" string) (req "uri" string) (req "origin" string)
        (opt "destination" string) (opt "content" encoding) in
    conv to_tuple of_tuple with_tuple
end

module Str = struct
  type t = {
    meth: string;
    uri: string;
    origin: string;
    destination: string option;
    content: string option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.meth, t.uri, t.origin, t.destination, t.content in
    let of_tuple v =
      let meth, uri, origin, destination, content = v in
      {meth; uri; origin; destination; content} in
    let with_tuple =
      obj5 (req "method" string) (req "uri" string) (req "origin" string)
        (opt "destination" string) (opt "content" string) in
    conv to_tuple of_tuple with_tuple
end
