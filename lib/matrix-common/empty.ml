open Json_encoding

module type JSON = sig
  type%accessor t = unit

  val encoding : t encoding
  val pp : t Fmt.t
end

module type QUERY = sig
  type%accessor t = unit

  val args : t -> (string * string list) list
end

module Json : JSON = struct
  type t = unit [@@deriving accessor]

  let encoding = unit
  let pp _ppf () = ()
end

module Query : QUERY = struct
  type t = unit [@@deriving accessor]

  let args () = []
end
