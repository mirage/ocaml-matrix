open Json_encoding

module Query = Empty.Query

let path = "/_matrix/client/versions"

module Response =
struct
  type t =
    { versions: string list
    ; unstable_features: (string * bool) list option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      (t.versions, t.unstable_features)
    in
    let of_tuple v =
      let (versions, unstable_features) = v in
      {versions; unstable_features}
    in
    let with_tuple =
      obj2
        (req "versions" (list string))
        (opt "unstable_features" (assoc bool))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = false
