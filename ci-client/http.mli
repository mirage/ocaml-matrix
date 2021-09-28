module Server : sig
  type scheme = [ `Http | `Https ]
  type t = {scheme: scheme; host: string; port: int}

  val pp : Format.formatter -> t -> unit
  val v : scheme -> string -> int -> t
  val to_uri : t -> string -> (string * string list) list option -> Uri.t
end

type 'a or_error = ('a, Matrix_ctos.Errors.t) result

val get :
  Server.t ->
  ?header:(string * string) list ->
  string ->
  (string * string list) list option ->
  'a Json_encoding.encoding ->
  string option ->
  'a or_error Lwt.t

val post :
  Server.t ->
  ?header:(string * string) list ->
  string ->
  (string * string list) list option ->
  'a ->
  'a Json_encoding.encoding ->
  'b Json_encoding.encoding ->
  string option ->
  'b or_error Lwt.t

val put :
  Server.t ->
  ?header:(string * string) list ->
  string ->
  (string * string list) list option ->
  'a ->
  'a Json_encoding.encoding ->
  'b Json_encoding.encoding ->
  string option ->
  'b or_error Lwt.t
