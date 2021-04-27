open Json_encoding

type t = {changed: string list option; left: string list option}
[@@deriving accessor]

let encoding =
  let to_tuple t = t.changed, t.left in
  let of_tuple v =
    let changed, left = v in
    {changed; left} in
  let with_tuple =
    obj2 (opt "changed" (list string)) (opt "left" (list string)) in
  conv to_tuple of_tuple with_tuple
