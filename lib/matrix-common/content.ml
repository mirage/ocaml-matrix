open Json_encoding

let content t =
  obj1 (req "content" t)
