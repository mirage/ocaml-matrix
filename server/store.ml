open Lwt.Infix

module Key =
struct
  include Mirage_kv.Key

  let add t v =
    let v =
    String.map
      (function
        | '/' -> '\000'
        | '\000' -> Fmt.failwith "%S is not a valid segment" v
        | c -> c) v
    in add t v

  let ( / ) = add
end

module Fs_store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

module Store :
sig
  type contents = Fs_store.contents

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
=
struct
  type contents = Fs_store.contents

  type key = Mirage_kv.key

  let to_Fs_key k =
    Mirage_kv.Key.segments k |>
    Fs_store.Key.v

  let to_kv_value = function
    | `Contents -> `Value
    | `Node -> `Dictionary

  type t = Fs_store.t

  type error = [ Mirage_kv.error | Fs_store.write_error ]

  let error = function
    | Ok v -> Ok v
    | Error e -> Error (e :> error)

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | #Fs_store.write_error as e -> Irmin.Type.pp Fs_store.write_error_t ppf e

  type write_error = [ error | Mirage_kv.write_error ]

  let write_error = function
    | Ok v -> Ok v
    | Error e -> Error (e :> write_error)

  let pp_write_error ppf = function
    | #error as e -> pp_error ppf e
    | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e

  let info () = Irmin.Info.v ~date:0L ~author:"ocaml-matrix.server" ""

  let catch_error f k =
    let exn e =
      match e with
      | Invalid_argument _ -> Lwt.return_error (`Not_found k :> error)
      | _ -> raise e
    in
    Lwt.catch f exn

  let disconnect t = Fs_store.repo t |> Fs_store.Repo.close

  let set t k v =
    let k = to_Fs_key k in
    Fs_store.set ~info t k v >|=
    write_error

  let exists t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_store.kind t k' >|=
      Option.map to_kv_value >>=
      Lwt.return_ok
    in
    catch_error f k

  let get t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_store.get t k' >>=
      Lwt.return_ok
    in
    catch_error f k

  let list t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_store.list t k' >|=
      List.map (fun (s, v) -> s, to_kv_value v) >>=
      Lwt.return_ok
    in
    catch_error f k

  let last_modified t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_store.last_modified t k' >>=
      (function
        | [] -> Fmt.pr "lol\n%!"; Lwt.return (Error (`Not_found k) |> error)
        | h::_ ->
          let time = Fs_store.Commit.info h |> Irmin.Info.date in
          Lwt.return_ok (0, time))
    in
    catch_error f k

  let remove t k =
    let k = to_Fs_key k in
    Fs_store.remove ~info t k >|=
    write_error

  let store () =
    let config = Irmin_git.config "/tmp/ocaml-matrix-string" in
    Fs_store.Repo.v config >>=
    Fs_store.master

  let digest _ _ = assert false

  let batch _ = assert false
end

let store =
  Lwt_main.run (Store.store ())









module Fs_event_store = Irmin_unix.Git.FS.KV(Irmin.Contents.Json_value)

module Event_store :
sig
  type contents = Fs_event_store.contents

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
=
struct
  type contents = Fs_event_store.contents

  type key = Mirage_kv.key

  let to_Fs_key k =
    Mirage_kv.Key.segments k |>
    Fs_event_store.Key.v

  let to_kv_value = function
    | `Contents -> `Value
    | `Node -> `Dictionary

  type t = Fs_event_store.t

  type error = [ Mirage_kv.error | Fs_event_store.write_error ]

  let error = function
    | Ok v -> Ok v
    | Error e -> Error (e :> error)

  let pp_error ppf = function
    | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
    | #Fs_event_store.write_error as e -> Irmin.Type.pp Fs_event_store.write_error_t ppf e

  type write_error = [ error | Mirage_kv.write_error ]

  let write_error = function
    | Ok v -> Ok v
    | Error e -> Error (e :> write_error)

  let pp_write_error ppf = function
    | #error as e -> pp_error ppf e
    | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e

  let info () = Irmin.Info.v ~date:0L ~author:"ocaml-matrix.server" ""

  let catch_error f k =
    let exn e =
      match e with
      | Invalid_argument _ -> Lwt.return_error (`Not_found k :> error)
      | _ -> raise e
    in
    Lwt.catch f exn

  let disconnect t = Fs_event_store.repo t |> Fs_event_store.Repo.close

  let set t k v =
    let k = to_Fs_key k in
    Fs_event_store.set ~info t k v >|=
    write_error

  let exists t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_event_store.kind t k' >|=
      Option.map to_kv_value >>=
      Lwt.return_ok
    in
    catch_error f k

  let get t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_event_store.get t k' >>=
      Lwt.return_ok
    in
    catch_error f k

  let list t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_event_store.list t k' >|=
      List.map (fun (s, v) -> s, to_kv_value v) >>=
      Lwt.return_ok
    in
    catch_error f k

  let last_modified t k =
    let k' = to_Fs_key k in
    let f () =
      Fs_event_store.last_modified t k' >>=
      (function
        | [] -> Fmt.pr "lol\n%!"; Lwt.return (Error (`Not_found k) |> error)
        | h::_ ->
          let time = Fs_event_store.Commit.info h |> Irmin.Info.date in
          Lwt.return_ok (0, time))
    in
    catch_error f k

  let remove t k =
    let k = to_Fs_key k in
    Fs_event_store.remove ~info t k >|=
    write_error

  let store () =
    let config = Irmin_git.config "/tmp/ocaml-matrix-event" in
    Fs_event_store.Repo.v config >>=
    Fs_event_store.master

  let digest _ _ = assert false

  let batch _ = assert false
end

let event_store =
  Lwt_main.run (Event_store.store ())
