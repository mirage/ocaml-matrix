open Json_encoding

type t = {kty: string; key_ops: string list; alg: string; k: string; ext: bool}
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
