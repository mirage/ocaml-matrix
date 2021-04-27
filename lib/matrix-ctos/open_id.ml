open Json_encoding
open Matrix_common
module Query = Empty.Query

module Response = struct
  type t = {
      access_token: string
    ; token_type: string
    ; matrix_server_name: string
    ; expires_in: int
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.access_token, t.token_type, t.matrix_server_name, t.expires_in in
    let of_tuple v =
      let access_token, token_type, matrix_server_name, expires_in = v in
      {access_token; token_type; matrix_server_name; expires_in} in
    let with_tuple =
      obj4
        (req "access_token" string)
        (req "token_type" string)
        (req "matrix_server_name" string)
        (req "expires_in" int) in
    conv to_tuple of_tuple with_tuple
end
