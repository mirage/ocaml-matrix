open Json_encoding

module Encrypted_file = struct
  module Jwk = struct
    type t = {
        kty: string
      ; key_ops: string list
      ; alg: string
      ; k: string
      ; ext: bool
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.kty, t.key_ops, t.alg, t.k, t.ext in
      let of_tuple v =
        let kty, key_ops, alg, k, ext = v in
        {kty; key_ops; alg; k; ext} in
      let with_tuple =
        obj5 (req "kty" string)
          (req "key_ops" (list string))
          (req "alg" string) (req "k" string) (req "ext" bool) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
      url: string
    ; key: Jwk.t
    ; iv: string
    ; hashes: (string * string) list
    ; v: string
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.url, t.key, t.iv, t.hashes, t.v in
    let of_tuple v =
      let url, key, iv, hashes, v = v in
      {url; key; iv; hashes; v} in
    let with_tuple =
      obj5 (req "url" string) (req "key" Jwk.encoding) (req "iv" string)
        (req "hashes" (assoc string))
        (req "v" string) in
    conv to_tuple of_tuple with_tuple
end

module Thumbnail = struct
  type t = {
      h: int option
    ; w: int option
    ; mimetype: string option
    ; size: int option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.h, t.w, t.mimetype, t.size in
    let of_tuple v =
      let h, w, mimetype, size = v in
      {h; w; mimetype; size} in
    let with_tuple =
      obj4 (opt "h" int) (opt "w" int) (opt "mimetype" string) (opt "size" int)
    in
    conv to_tuple of_tuple with_tuple
end

module Image = struct
  type t = {
      h: int option
    ; w: int option
    ; mimetype: string option
    ; size: int option
    ; thumbnail_url: string option
    ; thumbnail_file: Encrypted_file.t option
    ; thumbnail_info: Thumbnail.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.h
      , t.w
      , t.mimetype
      , t.size
      , t.thumbnail_url
      , t.thumbnail_file
      , t.thumbnail_info ) in
    let of_tuple v =
      let h, w, mimetype, size, thumbnail_url, thumbnail_file, thumbnail_info =
        v in
      {h; w; mimetype; size; thumbnail_url; thumbnail_file; thumbnail_info}
    in
    let with_tuple =
      obj7 (opt "h" int) (opt "w" int) (opt "mimetype" string) (opt "size" int)
        (opt "thumbnail_url" string)
        (opt "thumbnail_file" Encrypted_file.encoding)
        (opt "thumbnail_info" Thumbnail.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Video = struct
  type t = {
      duration: int option
    ; h: int option
    ; w: int option
    ; mimetype: string option
    ; size: int option
    ; thumbnail_url: string option
    ; thumbnail_file: Encrypted_file.t option
    ; thumbnail_info: Thumbnail.t option
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
        (opt "thumbnail_info" Thumbnail.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Location = struct
  type t = {
      thumbnail_url: string option
    ; thumbnail_file: Encrypted_file.t option
    ; thumbnail_info: Thumbnail.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.thumbnail_url, t.thumbnail_file, t.thumbnail_info in
    let of_tuple v =
      let thumbnail_url, thumbnail_file, thumbnail_info = v in
      {thumbnail_url; thumbnail_file; thumbnail_info} in
    let with_tuple =
      obj3
        (opt "thumbnail_url" string)
        (opt "thumbnail_file" Encrypted_file.encoding)
        (opt "thumbnail_info" Thumbnail.encoding) in
    conv to_tuple of_tuple with_tuple
end

module File = struct
  type t = {
      mimetype: string option
    ; size: int option
    ; thumbnail_url: string option
    ; thumbnail_file: Encrypted_file.t option
    ; thumbnail_info: Thumbnail.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.mimetype, t.size, t.thumbnail_url, t.thumbnail_file, t.thumbnail_info
    in
    let of_tuple v =
      let mimetype, size, thumbnail_url, thumbnail_file, thumbnail_info = v in
      {mimetype; size; thumbnail_url; thumbnail_file; thumbnail_info} in
    let with_tuple =
      obj5 (opt "mimetype" string) (opt "size" int)
        (opt "thumbnail_url" string)
        (opt "thumbnail_file" Encrypted_file.encoding)
        (opt "thumbnail_info" Thumbnail.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Audio = struct
  type t = {mimetype: string option; duration: int option; size: int option}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.mimetype, t.duration, t.size in
    let of_tuple v =
      let mimetype, duration, size = v in
      {mimetype; duration; size} in
    let with_tuple =
      obj3 (opt "mimetype" string) (opt "duration" int) (opt "size" int) in
    conv to_tuple of_tuple with_tuple
end
