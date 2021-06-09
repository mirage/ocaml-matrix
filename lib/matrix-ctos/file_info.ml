open Json_encoding

type t = {
  mimetype: string option;
  size: int option;
  thumbnail_url: string option;
  thumbnail_file: Encrypted_file.t option;
  thumbnail_info: Thumbnail_info.t option;
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
      (opt "thumbnail_info" Thumbnail_info.encoding) in
  conv to_tuple of_tuple with_tuple
