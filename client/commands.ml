open Http

module Client = struct
  open Matrix_ctos

  let gwen =
    Login.Post.Request.make
      ~auth:
        (Authentication.Password
           (V2
              (Authentication.Password.V2.make
                 ~identifier:
                   (Identifier.User (Identifier.User.make ~user:"gwen" ()))
                 ~password:"$crapaud$" ())))
      ()

  let login login =
    Login.Post.(
      Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login:");
      let response =
        Lwt_main.run
          (post "_matrix/client/r0/login"
             (Query.args (Query.make ()))
             login Request.encoding Response.encoding false) in
      Option.iter
        (fun token -> auth_token := token)
        (Response.get_access_token response);
      response)

  let public_rooms server =
    let args = Room_listing.Get_public_rooms.Query.make ?server () in
    Fmt.(
      pf stdout "%a\n%!"
        (styled `Cyan (styled `Bold string))
        "Get public rooms:");
    Lwt_main.run
      (get "_matrix/client/r0/publicRooms"
         (Room_listing.Get_public_rooms.Query.args args)
         Room_listing.Get_public_rooms.Response.encoding true)
end

module Server = struct
  open Matrix_stos

  let get_server_keys () =
    let open Key.Direct_query in
    let args = Query.make () in
    Fmt.(
      pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get public keys:");
    Lwt_main.run
      (get
         ~make_headers:(make_federation_headers "foo.bar" "ed25519:key1" "")
         "_matrix/key/v2/server" (Query.args args) Response.encoding true)

  let read_key file =
    let i = open_in file in
    let len = in_channel_length i in
    let bytes = Bytes.create len in
    really_input i bytes 0 len;
    match
      Rresult.R.error_msg_to_invalid_arg
        (X509.Private_key.decode_pem (Cstruct.of_bytes bytes))
    with
    | `ED25519 key -> key
    | _ -> raise @@ Invalid_argument "Not an ED25519 key"

  let gen_signature key str =
    Rresult.R.error_msg_to_invalid_arg
    @@ Base64.encode ~pad:false
    @@ Cstruct.to_string
    @@ Mirage_crypto_ec.Ed25519.sign ~key (Cstruct.of_string str)

  let public_rooms key =
    let open Public_rooms.Get_public_rooms in
    let args = Query.make () in
    Fmt.(
      pf stdout "%a\n%!"
        (styled `Cyan (styled `Bold string))
        "Get public rooms:");
    let json =
      Federation_request.make ~meth:"GET"
        ~uri:"/_matrix/federation/v1/publicRooms" ~origin:"ocaml-matrix"
        ~destination:"my.domain.name" () in
    let obj_key = read_key "/home/gwenaelle/.ssh/ed25519key.pem" in
    let response =
      Json_encoding.construct
        (Federation_request.encoding Response.encoding)
        json
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    let signed = gen_signature obj_key response in
    Lwt_main.run
      (get
         ~make_headers:(make_federation_headers "ocaml-matrix" key signed)
         "_matrix/federation/v1/publicRooms" (Query.args args) Response.encoding
         true)

  let join_room key room_id =
    let open Joining_rooms.Send_join.V2 in
    let event_id = "$" ^ Uuidm.(v `V4 |> to_string) ^ ":ocaml-matrix" in
    let uri = "/_matrix/federation/v2/send_join/" ^ room_id ^ "/" ^ event_id in
    let args = Query.make () in
    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Join a room:");
    let content =
      Matrix_common.Events.Pdu.make
        ~event:
          (`Room_event
            Matrix_common.Events.(
              Room_event.make
                ~event:
                  (Event.make
                     ~event_content:
                       Event_content.(Member (Member.make ~membership:Join ()))
                     ())
                ~sender:"@flibidi:ocaml-matrix" ~origin:"ocaml-matrix"
                ~origin_server_ts:((Unix.time () |> Float.to_int) * 1000)
                ~room_id ()))
        ~prev_events:[] ~depth:25 () in
    let json =
      Federation_request.make ~meth:"PUT" ~uri ~origin:"ocaml-matrix"
        ~destination:"my.domain.name" ~content () in
    let obj_key = read_key "/home/gwenaelle/.ssh/ed25519key.pem" in
    let response =
      Json_encoding.construct
        (Federation_request.encoding Request.encoding)
        json
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    let signed = gen_signature obj_key response in
    Lwt_main.run
      (put
         ~make_headers:(make_federation_headers "ocaml-matrix" key signed)
         uri (Query.args args) content Request.encoding Response.encoding true)
end
