open Matrix_common
open Store

let is_room_user room_id user_id =
  let%lwt tree = Store.tree store in
  let%lwt event_id =
    Store.Tree.find tree
      (Store.Key.v ["rooms"; room_id; "state"; "m.room.member"; user_id]) in
  match event_id with
  | None -> Lwt.return false
  | Some event_id -> (
      let%lwt state_event =
      Store.Tree.get tree
        (Store.Key.v ["events"; event_id]) in
      let state_event =
      Json_encoding.destruct Events.State_event.encoding
        (Ezjsonm.value_from_string state_event) in
    match Events.State_event.get_event_content state_event with
    | Member member -> (
      match Events.Event_content.Member.get_membership member with
      | Join -> Lwt.return true
      | _ -> assert false)
    | _ -> assert false)

let time () = Unix.time () |> Float.to_int |> ( * ) 1000

let info (t : Common_routes.t) ?(message = "") () =
  Irmin.Info.v
    ~date:(Int64.of_float (Unix.gettimeofday ()))
    ~author:t.server_name message

let compute_hash_and_sign (t: Common_routes.t) pdu =
  let open Events in
  (* ensure already computed hash does not interfere *)
  let pdu = Pdu.set_hashes pdu None in
  (* generate hash *)
  let string_pdu =
    Json_encoding.construct Pdu.encoding pdu
    |> Json_encoding.canonize
    |> Ezjsonm.value_to_string in
  let sha256 =
    Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string string_pdu)
    |> Cstruct.to_string
    |> Base64.encode_string ~pad:false ~alphabet:(Base64.uri_safe_alphabet)
  in
  let hash = Pdu.Hashes.make ~sha256 () in
  let pdu = Pdu.set_hashes pdu (Some hash) in
  (* sign pdu *)
  let string_pdu =
    Json_encoding.construct Pdu.redact pdu
    |> Json_encoding.canonize
    |> Ezjsonm.value_to_string in
  let signature =
    Mirage_crypto_ec.Ed25519.sign ~key:t.priv_key
      (Cstruct.of_string string_pdu)
    |> Cstruct.to_string
    |> Base64.encode_string ~pad:false in
  let signatures = Pdu.get_signatures pdu in
  let signatures =
    if List.mem_assoc t.server_name signatures
    then
      List.map
        (fun (server_name, l) ->
          let l = if server_name = t.server_name then ("ed25519:" ^ t.key_name, signature)::l else l in
          server_name, l) signatures
    else
      (t.server_name, ["ed25519:" ^ t.key_name, signature])::signatures
  in
  let pdu =
    Pdu.set_signatures pdu signatures in
  pdu

let compute_event_reference_hash pdu =
  let open Events in
  (* generate hash *)
  let string_pdu =
    Json_encoding.construct Pdu.redact pdu
    |> Json_encoding.canonize
    |> Ezjsonm.value_to_string in
  let sha256 =
    Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string string_pdu)
    |> Cstruct.to_string
    |> Base64.encode_string ~pad:false ~alphabet:(Base64.uri_safe_alphabet)
  in
  sha256

(* Use older/replaced events once they are implemented *)
let get_room_prev_events ?(limit=10) room_id =
  let%lwt tree = Store.tree store in
  let%lwt state_tree =
    Store.Tree.get_tree tree (Store.Key.v ["rooms"; room_id; "state"]) in
  let%lwt l =
    Store.Tree.fold
      ~contents:(fun _ event_id l ->
        let open Events in
        let%lwt json = Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
        let event =
          Ezjsonm.from_string json
          |> Json_encoding.destruct Pdu.encoding in
        Lwt.return ((Pdu.get_depth event, "$" ^ event_id) :: l))
      state_tree [] in
  let prev_events =
    List.sort (fun (d1, _) (d2, _) -> d2 - d1 ) l
    |> List.filteri (fun i _ -> i < limit)
  in
  let max_depth, _ = List.hd prev_events in
  let prev_events = List.split prev_events |> snd
  in
  Lwt.return (max_depth, prev_events)
