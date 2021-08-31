open Matrix_common
open Store

let is_room_user room_id user_id =
  let%lwt tree = Store.tree store in
  let%lwt state_event =
    Store.Tree.find tree
      (Store.Key.v ["rooms"; room_id; "state"; "m.room.member"; user_id]) in
  match state_event with
  | None -> Lwt.return false
  | Some state_event -> (
    let state_event =
      Json_encoding.destruct Events.State_event.encoding
        (Ezjsonm.value_from_string state_event) in
    match Events.State_event.get_event_content state_event with
    | Member member -> (
      match Events.Event_content.Member.get_membership member with
      | Join -> Lwt.return true
      | _ -> assert false)
    | _ -> assert false)

let time () = Unix.time () |> Float.to_int

let info (t : Common_routes.t) ?(message = "") () =
  Irmin.Info.v
    ~date:(Int64.of_float (Unix.gettimeofday ()))
    ~author:t.server_name message
