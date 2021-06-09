open Json_encoding
open Matrix_common
module Query = Empty.Query

module Response = struct
  type t = {
    (* Should all be required, but tldr: anwser can be null and `option` didnt work *)
    username: string option;
    password: string option;
    uris: string list option;
    ttl: int option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.username, t.password, t.uris, t.ttl in
    let of_tuple v =
      let username, password, uris, ttl = v in
      {username; password; uris; ttl} in
    let with_tuple =
      obj4 (opt "username" string) (opt "password" string)
        (opt "uris" (list string))
        (opt "ttl" int) in
    conv to_tuple of_tuple with_tuple
end
