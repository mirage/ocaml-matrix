open Json_encoding

type t = {
  thumbnail_url: string option;
  thumbnail_file: Encrypted_file.t option;
  thumbnail_info: Thumbnail_info.t option;
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
      (opt "thumbnail_info" Thumbnail_info.encoding) in
  conv to_tuple of_tuple with_tuple
