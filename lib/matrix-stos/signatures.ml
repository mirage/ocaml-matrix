open Json_encoding

let gen_signature key encoding obj =
  let encoded = construct encoding obj |> canonize |> Ezjsonm.value_to_string in
  Fmt.pr "XXX%sXXX\n" encoded;
  Rresult.R.error_msg_to_invalid_arg @@ Base64.encode ~pad:false @@ Cstruct.to_string @@ Mirage_crypto_ec.Ed25519.sign ~key (Cstruct.of_string encoded)

let encoding keys encoding =
  let to_tuple t = t, List.map (fun (server, keys) -> server, List.map (fun (id, key) -> id, gen_signature key encoding t) keys) keys in
  let of_tuple v =
    let obj, _signatures = v in
    obj in
  let with_tuple = merge_objs encoding (obj1 (req "signatures" (assoc (assoc string)))) in
  conv to_tuple of_tuple with_tuple
