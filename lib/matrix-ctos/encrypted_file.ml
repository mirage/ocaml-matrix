open Json_encoding

type t =
  { url: string
  ; key: Jwk.t
  ; iv: string
  ; hashes: (string * string) list
  ; v: string
  } [@@deriving accessor]

let encoding =
  let to_tuple t =
    t.url, t.key, t.iv, t.hashes, t.v
  in
  let of_tuple v =
    let url, key, iv, hashes, v = v in
    { url; key; iv; hashes; v }
  in
  let with_tuple =
    obj5
      (req "url" string)
      (req "key" Jwk.encoding)
      (req "iv" string)
      (req "hashes" (assoc string))
      (req "v" string)
  in
  conv to_tuple of_tuple with_tuple
