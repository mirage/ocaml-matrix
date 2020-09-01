open Json_encoding

type t =
  { mimetype: string option
  ; duration: int option
  ; size: int option
  } [@@deriving accessor]

let encoding =
  let to_tuple t =
    t.mimetype, t.duration, t.size
  in
  let of_tuple v =
    let mimetype, duration, size = v in
    { mimetype; duration; size }
  in
  let with_tuple =
    obj3
      (opt "mimetype" string)
      (opt "duration" int)
      (opt "size" int)
  in
  conv to_tuple of_tuple with_tuple
