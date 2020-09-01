open Json_encoding

type t =
  { h: int option
  ; w: int option
  ; mimetype: string option
  ; size: int option
  } [@@deriving accessor]

let encoding =
  let to_tuple t =
    t.h, t.w, t.mimetype, t.size
  in
  let of_tuple v =
    let h, w, mimetype, size = v in
    { h; w; mimetype; size }
  in
  let with_tuple =
    obj4
      (opt "h" int )
      (opt "w" int )
      (opt "mimetype" string)
      (opt "size" int)
  in
  conv to_tuple of_tuple with_tuple
