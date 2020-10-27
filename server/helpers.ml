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
    Lwt.return (`Not_implemented, "Not implemented", Some "text/html")
  in
  false, f

let deprecated =
  let f _ _ _ _ =
    Lwt.return (`Moved_permanently, "Not implemented", Some "text/html")
  in
  false, f

let username_to_user_id username =
  "@" ^ username ^ ":" ^ Const.homeserver

let homeserver_of_user_id user_id =
  List.nth (String.split_on_char ':' user_id) 1

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
    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
    | Ok () -> f (Some token))

let get_account_data user_id _since =
  let push_rules =
  Event.Event
    (Event.Event.Push_rules
      (Event.Event.Push_rules.make
        ~content:[]
        ~override:[]
        ~room:[]
        ~sender:[]
        ~underride:[]
        ()))
  in
  Store.list store Key.(v "users" / user_id / "data") >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok data_types ->
      Lwt_list.filter_map_p
        (fun (data_type, _) ->
          Store.get store Key.(v "users" / user_id / "data" / data_type) >>=
          (function
            | Error _ -> Lwt.return_none
            | Ok data_id ->
              Event_store.get event_store Key.(v data_id) >>=
              (function
                | Error _ -> Lwt.return_none
                | Ok content ->
                  let event =
                    Event.Custom
                      (Event.Custom.make
                        ~type_id:data_type
                        ~content
                        ())
                  in
                  Lwt.return_some event))) data_types >>=
      (fun account_data ->
         Lwt.return_ok (push_rules::account_data, false)))
