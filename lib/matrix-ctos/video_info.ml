open Json_encoding

type t = {
    duration: int option
  ; h: int option
  ; w: int option
  ; mimetype: string option
  ; size: int option
  ; thumbnail_url: string option
  ; thumbnail_file: Encrypted_file.t option
  ; thumbnail_info: Thumbnail_info.t option
}
[@@deriving accessor]

let encoding =
  let to_tuple t =
    ( t.duration
    , t.h
    , t.w
    , t.mimetype
    , t.size
    , t.thumbnail_url
    , t.thumbnail_file
    , t.thumbnail_info ) in
  let of_tuple v =
    let ( duration
        , h
        , w
        , mimetype
        , size
        , thumbnail_url
        , thumbnail_file
        , thumbnail_info ) =
      v in
    {
      duration
    ; h
    ; w
    ; mimetype
    ; size
    ; thumbnail_url
    ; thumbnail_file
    ; thumbnail_info
    } in
  let with_tuple =
    obj8 (opt "duration" int) (opt "h" int) (opt "w" int)
      (opt "mimetype" string) (opt "size" int)
      (opt "thumbnail_url" string)
      (opt "thumbnail_file" Encrypted_file.encoding)
      (opt "thumbnail_info" Thumbnail_info.encoding) in
  conv to_tuple of_tuple with_tuple
