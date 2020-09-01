(* Changed mirage_kv signature to accept any contents *)

module Key:
sig
  type t = Mirage_kv.key
  val empty : t
  val v : string -> t
  val add : t -> string -> t
  val ( / ) : t -> string -> t
  val append : t -> t -> t
  val ( // ) : t -> t -> t
  val segments : t -> string list
  val basename : t -> string
  val parent : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val to_string : t -> string
end

module Store:
sig
  type contents = Irmin.Contents.String.t

  (* Changed mirage_kv signature to accept any contents *)
  type nonrec error = private [> Mirage_kv.error ]
  val pp_error : error Fmt.t
  type t
  val disconnect : t -> unit Lwt.t
  type key = Key.t
  val exists :
    t -> key -> ([ `Dictionary | `Value ] option, error) result Lwt.t
  val get : t -> key -> (contents, error) result Lwt.t
  val list :
    t ->
    key -> ((string * [ `Dictionary | `Value ]) list, error) result Lwt.t
  val last_modified : t -> key -> (int * int64, error) result Lwt.t
  val digest : t -> key -> (string, error) result Lwt.t
  type nonrec write_error = private [> Mirage_kv.write_error ]
  val pp_write_error : write_error Fmt.t
  val set : t -> key -> contents -> (unit, write_error) result Lwt.t
  val remove : t -> key -> (unit, write_error) result Lwt.t
  val batch : t -> ?retries:int -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val store : unit -> t Lwt.t
end

val store : Store.t

module Event_store:
sig
  type contents = Irmin.Contents.Json_value.t

  (* Changed mirage_kv signature to accept any contents *)
  type nonrec error = private [> Mirage_kv.error ]
  val pp_error : error Fmt.t
  type t
  val disconnect : t -> unit Lwt.t
  type key = Key.t
  val exists :
    t -> key -> ([ `Dictionary | `Value ] option, error) result Lwt.t
  val get : t -> key -> (contents, error) result Lwt.t
  val list :
    t ->
    key -> ((string * [ `Dictionary | `Value ]) list, error) result Lwt.t
  val last_modified : t -> key -> (int * int64, error) result Lwt.t
  val digest : t -> key -> (string, error) result Lwt.t
  type nonrec write_error = private [> Mirage_kv.write_error ]
  val pp_write_error : write_error Fmt.t
  val set : t -> key -> contents -> (unit, write_error) result Lwt.t
  val remove : t -> key -> (unit, write_error) result Lwt.t
  val batch : t -> ?retries:int -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val store : unit -> t Lwt.t
end

val event_store : Event_store.t
