open Json_encoding

type t = {
  meth: string;
  uri: string;
  origin: string;
  destination: string option;
  content: Ezjsonm.value option;
}
[@@deriving accessor]

let encoding =
  let to_tuple t = t.meth, t.uri, t.origin, t.destination, t.content in
  let of_tuple v =
    let meth, uri, origin, destination, content = v in
    {meth; uri; origin; destination; content} in
  let with_tuple =
    obj5 (req "method" string) (req "uri" string) (req "origin" string)
      (opt "destination" string) (opt "content" any) in
  conv to_tuple of_tuple with_tuple
