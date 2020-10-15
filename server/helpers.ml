open Matrix_ctos
open Json_encoding
open Store
open Lwt.Infix

let id () = Uuidm.(v `V4 |> to_string)

let room_id () = "!" ^ (id ()) ^ ":" ^ Const.homeserver

let event_id () = "$" ^ (id ())

let time () = Unix.time () |> Float.to_int

let error errcode error =
  let open Errors in
  let error = Error (Error.make ~errcode ~error ()) in
  construct encoding error |>
  Ezjsonm.value_to_string

let ( >?= ) f g =
  Lwt.bind f
  (function
  | Ok r -> g r
  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure"))

let placeholder =
  let f _ _ _ _ =
    Lwt.return (`Not_implemented, "")
  in
  false, f

let deprecated =
  let f _ _ _ _ =
    Lwt.return (`Moved_permanently, "")
  in
  false, f

let username_to_user_id username =
  "@" ^ username ^ ":" ^ Const.homeserver

let get_logged_user = function
  | None -> Lwt.return_ok None
  | Some token ->
    Store.exists store Key.(v "tokens" / token) >>=
    (function
      | Error _ -> Lwt.return_error ()
      | Ok None -> Lwt.return_ok None
      | Ok (Some _) ->
        Store.get store Key.(v "tokens" / token) >>=
        (function
          | Ok user -> Lwt.return_ok (Some user)
          | _ -> Lwt.return_error ()))

let get_username user_id =
  Store.get store Key.(v "users" / user_id / "displayname")

let create_token f username =
  let token = "auth_token_for_" ^ username in
  Store.set store Key.(v "tokens" / token) username >>=
  (function
    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
    | Ok () -> f (Some token))
