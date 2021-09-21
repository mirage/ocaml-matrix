open Lwt
open Lwt.Syntax
open Matrix_ctos
module Room = Matrix_ctos.Room

(* When the server returns a 429 Too Many Requests, instead of failing the request 
   is re-tried after a delay: either the value as specified by the error message or 1 
   second by default. *)
let with_retry_loop fn =
  let rec loop () =
    let* result = fn () in
    match result with
    | Error Matrix_ctos.Errors.(Rate_limited v) ->
      let delay_ms =
        Matrix_ctos.Errors.Rate_limited.get_retry_after_ms v
        |> Option.value ~default:1000 in
      let* () = Lwt_unix.sleep (Float.of_int delay_ms /. 1000.) in
      loop ()
    | Error e -> raise (Failure (Fmt.to_to_string Matrix_ctos.Errors.pp e))
    | Ok v -> Lwt.return v in
  loop ()

let with_pool ~job ~pool fn =
  let switch = Current.Switch.create ~label:"matrix-client-pool" () in
  Lwt.finalize
    (fun () ->
      let* () = Current.Job.use_pool ~switch job pool in
      fn ())
    (fun () -> Current.Switch.turn_off switch)

(* Override Http module with the retry loop and pooling *)
module Http = struct
  module Server = Http.Server

  let post
      ~job
      ~pool
      server
      ?header
      path
      args
      value
      request_encoding
      response_encoding
      auth_token =
    with_pool ~job ~pool @@ fun () ->
    with_retry_loop (fun () ->
        Http.post server ?header path args value request_encoding
          response_encoding auth_token)

  let get ~job ~pool server ?header path args response_encoding needs_auth =
    with_pool ~job ~pool @@ fun () ->
    with_retry_loop (fun () ->
        Http.get server ?header path args response_encoding needs_auth)

  let put
      ~job
      ~pool
      server
      ?header
      path
      args
      value
      request_encoding
      response_encoding
      auth_token =
    with_pool ~job ~pool @@ fun () ->
    with_retry_loop (fun () ->
        Http.put server ?header path args value request_encoding
          response_encoding auth_token)
end

module Token = struct
  type t = {
    mutex: Lwt_mutex.t;
    server: Http.Server.t;
    login: Login.Post.Request.t;
    mutable token: (string * int) option; (* token and ref count *)
  }

  let v ~server ~login () =
    {server; login; mutex= Lwt_mutex.create (); token= None}

  let login ~job ~pool server login =
    Current.Job.log job "Login to %a" Http.Server.pp server;
    let open Login.Post in
    Http.post ~job ~pool server "_matrix/client/r0/login" None login
      Request.encoding Response.encoding None

  let login_and_mutate ~job ~pool t =
    let+ response = login ~job ~pool t.server t.login in
    let token = Login.Post.Response.get_access_token response |> Option.get in
    t.token <- Some (token, 1);
    token

  let logout ~job ~pool server auth_token =
    Current.Job.log job "Logout from server";
    let open Logout.Logout in
    Http.post ~job ~pool server "_matrix/client/r0/logout" None
      (Request.make ()) Request.encoding Response.encoding auth_token

  let logout_and_mutate ~job ~pool t token =
    t.token <- None;
    logout ~job ~pool t.server (Some token) |> Lwt.map ignore

  (** [with_token ~job ~pool t fn] ensures a login token is available before calling [fn] with its value. *)
  let with_token ~job ~pool t fn =
    Lwt.finalize
      (fun () ->
        let* token =
          (* locking ensures a single thread has access to the ref count at the same time*)
          Lwt_mutex.with_lock t.mutex (fun () ->
              (* increment the reference count, login if no token is available *)
              match t.token with
              | None -> login_and_mutate ~job ~pool t
              | Some (token, n) ->
                t.token <- Some (token, n + 1);
                Lwt.return token)
        in
        (* use token with the user function *)
        fn token)
      (fun () ->
        (* locking as we don't want concurrent access to the token while logging out *)
        Lwt_mutex.with_lock t.mutex (fun () ->
            match t.token with
            | None -> Lwt.return_unit
            | Some (token, 1) -> logout_and_mutate ~job ~pool t token
            | Some (token, n) ->
              t.token <- Some (token, n - 1);
              Lwt.return_unit))
end

type t = {
  server: Http.Server.t;
  device: string option;
  user: string;
  pwd: string;
  token: Token.t;
  pool: unit Current.Pool.t;
}

let make_login device_id user password =
  let identifier = Identifier.User (Identifier.User.make ~user ()) in
  let auth =
    Authentication.Password
      (V2 (Authentication.Password.V2.make ~identifier ~password ())) in
  Login.Post.Request.make ~auth ?device_id ()

let v ?(max_connections = 8) ~server ~device ~user ~pwd () =
  let login = make_login device user pwd in
  let token = Token.v ~server ~login () in
  {
    server;
    device;
    user;
    pwd;
    token;
    pool=
      Current.Pool.create
        ~label:("matrix-client-" ^ server.host)
        max_connections;
  }

let resolve_alias ~job ~pool server room_alias =
  Current.Job.log job "Resolving alias `%s` for room name" room_alias;
  let open Room.Resolve_alias in
  Http.get ~job ~pool server
    (Fmt.str "/_matrix/client/r0/directory/room/%s" room_alias)
    None Response.encoding None

let send_message ~job ~pool server auth_token txn_id message room_id =
  Current.Job.log job "Sending message to room `%s`" room_id;
  let open Room_event.Put.Message_event in
  Http.put ~job ~pool server
    (Fmt.str "/_matrix/client/r0/rooms/%s/send/%s/%s" room_id "m.room.message"
       txn_id)
    None message Request.encoding Response.encoding auth_token
  >|= ignore

let send_state
    ~job ~pool server auth_token room_id (state_kind, state, state_key) =
  Current.Job.log job "Sending state to room `%s`: %s" room_id state_kind;
  let open Room_event.Put.State_event in
  Http.put ~job ~pool server
    (Fmt.str "/_matrix/client/r0/rooms/%s/state/%s/%s" room_id state_kind
       state_key)
    None state Request.encoding Response.encoding auth_token
  >|= ignore

let post ~job ~room_id ctx message =
  let pool = ctx.pool in
  Token.with_token ~job ~pool ctx.token @@ fun auth_token ->
  let txn_id = Uuidm.(v `V4 |> to_string) in
  let message = Room_event.Put.Message_event.Request.make ~event:message () in
  let+ () =
    send_message ~job ~pool ctx.server (Some auth_token) txn_id message room_id
  in
  Ok ()

type settings = {
  name: string;
  topic: string;
  power_level_content_override:
    Matrix_common.Events.Event_content.Power_levels.t option;
}

let create_room
    ~job
    ~pool
    server
    auth_token
    room
    {name; topic; power_level_content_override} =
  Current.Job.log job "Creating room `%s`" room;
  let open Room.Create in
  Http.post ~job ~pool server "/_matrix/client/r0/createRoom" None
    (Request.make ~visibility:Public ~room_alias_name:room ~name ~topic
       ?power_level_content_override ())
    Request.encoding Response.encoding auth_token

let update_room
    ~job
    ~pool
    server
    auth_token
    room_id
    {name; topic; power_level_content_override} =
  Current.Job.log job "Updating room `%s`" room_id;
  let state_name =
    Room_event.Put.State_event.Request.make
      ~event:(Name (Matrix_common.Events.Event_content.Name.make ~name ()))
      () in
  let* () =
    send_state ~job ~pool server auth_token room_id
      ("m.room.name", state_name, "")
  in
  let state_topic =
    Room_event.Put.State_event.Request.make
      ~event:(Topic (Matrix_common.Events.Event_content.Topic.make ~topic ()))
      () in
  let* () =
    send_state ~job ~pool server auth_token room_id
      ("m.room.topic", state_topic, "")
  in
  let power_level_content_override =
    Option.value power_level_content_override
      ~default:(Matrix_common.Events.Event_content.Power_levels.make ()) in
  let state_power_levels =
    Room_event.Put.State_event.Request.make
      ~event:(Power_levels power_level_content_override) () in

  send_state ~job ~pool server auth_token room_id
    ("m.room.power_levels", state_power_levels, "")

let get_room ~job ~alias ~settings ctx =
  let pool = ctx.pool in
  Token.with_token ~job ~pool ctx.token @@ fun auth_token ->
  (* we look for the room *)
  let* existing_room_alias =
    Lwt.catch
      (fun () ->
        let+ alias =
          resolve_alias ~job ~pool ctx.server
            ("#" ^ alias ^ ":" ^ ctx.server.host)
        in
        Room.Resolve_alias.Response.get_room_id alias)
      (fun _ -> Lwt.return_none)
  in
  let+ room_id =
    match existing_room_alias with
    | None ->
      let+ create_room_response =
        create_room ~job ~pool ctx.server (Some auth_token) alias settings
      in
      Room.Create.Response.get_room_id create_room_response
    | Some room_id ->
      Current.Job.log job
        "Room already exists, making sure it has the correct settings.";
      let+ () =
        update_room ~job ~pool ctx.server (Some auth_token) room_id settings
      in
      room_id
  in
  Current.Job.log job "Room id: %s" room_id;
  Ok room_id
