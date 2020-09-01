module type JSON  =
sig
  [%%accessor:
    type t = unit ]
  val encoding: t Json_encoding.encoding
  val pp: t Fmt.t
end

module type QUERY =
sig
  [%%accessor:
    type t = unit ]
  val args: t -> (string * string list) list
end

module Json : JSON =
struct
  type t = unit [@@deriving accessor]

  let encoding = Json_encoding.unit

  let pp _ppf () = ()
end

module Query : QUERY =
struct
  type t = unit [@@deriving accessor]

  let args () = []
end
