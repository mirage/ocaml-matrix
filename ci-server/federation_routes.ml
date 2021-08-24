open Helper
open Middleware
open Matrix_stos
open Common_routes

let placeholder _ = assert false

let sign t encoding =
  Signatures.encoding
  [t.server_name, ["ed25519:foo_bar", t.priv_key]]
  encoding

module Key = struct

  module V2 = struct
    let direct_query t request =
      let open Key.Direct_query in
      let _key_id = Dream.param "key_id" request in
      match
        Base64.encode ~pad:false
          (Cstruct.to_string @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct t.pub_key)
      with
      | Ok base64_key ->
        let server_name = t.server_name in
        let response =
          Response.make ~server_name
            ~verify_keys:
              ["ed25519:foo_bar", Response.Verify_key.make ~key:base64_key ()]
            ~old_verify_keys:[]
            ~valid_until_ts:((time () + 60) * 1000)
            ()
          |> Json_encoding.construct (sign t Response.encoding)
          |> Ezjsonm.value_to_string in
        Dream.json response
      | Error _ ->
        Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKOWN"}|}
  end
end

let router (t: Common_routes.t) =
  Dream.router
    [
      Dream.scope "/_matrix" []
        [
          Dream.scope "/federation" []
            [
              Dream.scope "/v1" []
                [
                  Dream.get "/version" placeholder;
                  Dream.put "/3pid/onbind" placeholder;
                  Dream.get "/openid/userinfo" placeholder;
                  Dream.scope "" [is_logged_server]
                    [
                      Dream.put "/send/:txn_id" placeholder;
                      Dream.get "/event_auth/:room_id/:event_id" placeholder;
                      Dream.get "/backfill/:room_id" placeholder;
                      Dream.post "/get_missing_events/:room_id" placeholder;
                      Dream.get "/state/:room_id" placeholder;
                      Dream.get "/state_ids/:room_id" placeholder;
                      Dream.get "/event/:event_id" placeholder;
                      Dream.get "/make_join/:room_id/:user_id" placeholder;
                      Dream.put "/send_join/:room_id/:event_id" placeholder;
                      Dream.put "/invite/:room_id/:event_id" placeholder;
                      Dream.get "/make_leave/:room_id/:user_id" placeholder;
                      Dream.put "/send_leave/:room_id/:event_id" placeholder;
                      Dream.put "/exchange_third_party_invite/:room_id"
                        placeholder; Dream.get "/publicRooms" placeholder;
                      Dream.post "/publicRooms" placeholder;
                      Dream.scope "/query" []
                        [
                          Dream.get "/:query_type" placeholder;
                          Dream.get "/directory" placeholder;
                          Dream.get "/profile" placeholder;
                        ];
                      Dream.scope "/user" []
                        [
                          Dream.get "/devices/:user_id" placeholder;
                          Dream.scope "/keys" []
                            [
                              Dream.post "/claim" placeholder;
                              Dream.post "/query" placeholder;
                            ];
                        ];
                    ];
                ];
              Dream.scope "/v2" []
                [
                  Dream.scope "" [is_logged_server]
                    [
                      Dream.put "/send_join/:room_id/:event_id" placeholder;
                      Dream.put "/invite/:room_id/:event_id" placeholder;
                      Dream.put "/send_leave/:room_id/:event_id" placeholder;
                    ];
                ];
            ];
          Dream.scope "/key/v2" []
            [
              Dream.get "/server" placeholder;
              Dream.get "/server/:key_id" (Key.V2.direct_query t);
              Dream.get "/query/:server_name/:key_id" placeholder;
              Dream.post "/query" placeholder;
            ];
        ];
    ]
