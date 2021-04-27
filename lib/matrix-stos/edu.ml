open Json_encoding

type t = {edu_type: string; content: Repr.value} [@@deriving accessor]

let encoding =
  let to_tuple t = t.edu_type, t.content in
  let of_tuple v =
    let edu_type, content = v in
    {edu_type; content} in
  let with_tuple = obj2 (req "edu_type" string) (req "content" any) in
  conv to_tuple of_tuple with_tuple
