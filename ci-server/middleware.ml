open Store
open Matrix_stos

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
  module Paf = Paf_mirage.Make (Time) (Stack)
  module Helper = Helper.Make (Pclock) (Time) (Stack)

  let body =
    Dream.new_local ~name:"body" ~show_value:(fun _ -> "body promise") ()

  (* FIX ME: This is recreated each time we do a make, I need to find a way to
     avoid that. Creating a small module used as a functor everywhere should be
     the solution *)
  let body request =
    match Dream.local body request with
    | Some body -> Lwt.return (request, body)
    | None ->
      let body' = Dream.body request in
      Lwt.return (Dream.with_local body body' request, body')

  let logged_user =
    Dream.new_local ~name:"logged_user" ~show_value:(fun s -> s) ()

  let logged_device =
    Dream.new_local ~name:"logged_device" ~show_value:(fun s -> s) ()

  let clean_token token =
    if Astring.String.is_prefix ~affix:"Bearer " token then
      match Astring.String.cut ~sep:" " token with
      | Some (_, r) -> Some r
      | None -> None
    else None

  let unkown_token =
    Dream.json ~status:`Unauthorized
      {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched"}|}

  let is_valid_token token_tree =
    let%lwt expires_at =
      Store.Tree.get token_tree @@ Store.Key.v ["expires_at"] in
    let expires_at = Float.of_string expires_at in
    let current_time = Unix.gettimeofday () in
    Lwt.return (expires_at > current_time)

  (** Notes:
    - In cruel need of error handling
  *)
  let is_logged (t : Common_routes.t) handler request =
    let token =
      match Option.bind (Dream.header "Authorization" request) clean_token with
      | None -> Dream.query "access_token" request
      | token -> token in
    match token with
    | None ->
      Dream.json ~status:`Unauthorized
        {|{"errcode": "M_MISSING_TOKEN", "error": "No access token was specified"}|}
    | Some token -> (
      let%lwt tree = Store.tree t.store in
      (* fetch the token *)
      let%lwt token_tree =
        Store.Tree.find_tree tree @@ Store.Key.v ["tokens"; token] in
      match token_tree with
      | None -> unkown_token
      | Some token_tree -> (
        let%lwt is_valid = is_valid_token token_tree in
        if not is_valid then unkown_token
        else
          (* fetch the device *)
          let%lwt device = Store.Tree.get token_tree @@ Store.Key.v ["device"] in
          let%lwt device_tree =
            Store.Tree.find_tree tree (Store.Key.v ["devices"; device]) in
          match device_tree with
          | None -> unkown_token
          | Some device_tree -> (
            (* fetch the user *)
            let%lwt user_id =
              Store.Tree.get device_tree @@ Store.Key.v ["user_id"] in
            let%lwt user_tree =
              Store.Tree.find_tree tree (Store.Key.v ["users"; user_id]) in
            match user_tree with
            | None -> unkown_token
            | Some user_tree -> (
              (* verify the device is still listed in the user's devices *)
              let%lwt user_device =
                Store.Tree.find_tree user_tree (Store.Key.v ["devices"; device])
              in
              match user_device with
              | None -> unkown_token
              | Some _ ->
                (* verify the token is still the actual device token *)
                let%lwt device_token =
                  Store.Tree.get device_tree (Store.Key.v ["token"]) in
                if device_token <> token then unkown_token
                else
                  handler
                    (Dream.with_local logged_device device
                       (Dream.with_local logged_user user_id request))))))

  let logged_server =
    Dream.new_local ~name:"logged_server" ~show_value:(fun s -> s) ()

  let clean_auth auth =
    if Astring.String.is_prefix ~affix:"X-Matrix " auth then
      let auth = snd @@ Option.get @@ Astring.String.cut ~sep:" " auth in
      match Astring.String.cuts ~sep:"," auth with
      | [origin; key; signature] ->
        if
          Astring.String.is_prefix ~affix:"origin=" origin
          && Astring.String.is_prefix ~affix:"key=" key
          && Astring.String.is_prefix ~affix:"sig=" signature
        then
          let origin = snd @@ Option.get @@ Astring.String.cut ~sep:"=" origin in
          let key =
            Astring.String.trim ~drop:(function '"' -> true | _ -> false)
            @@ snd
            @@ Option.get
            @@ Astring.String.cut ~sep:"=" key in
          let signature =
            Astring.String.trim ~drop:(function '"' -> true | _ -> false)
            @@ snd
            @@ Option.get
            @@ Astring.String.cut ~sep:"=" signature in
          Some (origin, key, signature)
        else None
      | _ -> None
    else None

  let unkown_key key =
    Dream.json ~status:`Unauthorized
      (Fmt.str
         {|{"errcode": "M_UNAUTHORIZED", "error": "Failed to find any valid key for [%s]"}|}
         key)

  (** Notes:
    - Use the right key type
  *)
  let is_valid_signature origin destination signature key request =
    let meth = Dream.method_to_string @@ Dream.method_ request in
    let uri = Dream.target request in
    let%lwt request, body = body request in
    let%lwt body = body in
    let content =
      if body = "" then None else Some (Ezjsonm.value_from_string body) in
    let json =
      Federation_request.make ~meth ~uri ?content ~origin ~destination ()
      |> Json_encoding.construct Federation_request.encoding
      |> Json_encoding.sort
      |> Ezjsonm.value_to_string in
    match Base64.decode ~pad:false key with
    | Error _ -> Lwt.return (request, false)
    | Ok key -> (
      match Mirage_crypto_ec.Ed25519.pub_of_cstruct (Cstruct.of_string key) with
      | Error _ -> Lwt.return (request, false)
      | Ok key -> (
        match Base64.decode ~pad:false signature with
        | Error _ -> Lwt.return (request, false)
        | Ok signature ->
          let verify =
            Mirage_crypto_ec.Ed25519.verify ~key
              (Cstruct.of_string signature)
              ~msg:(Cstruct.of_string json) in
          Lwt.return (request, verify)))

  (** Notes:
    - Maybe do some server name resolution in order to avoid false negatives
    - Upgrade the key fetching with proper retries and using several notary
      servers.
  *)
  let is_logged_server (t : Common_routes.t) handler request =
    match Option.bind (Dream.header "Authorization" request) clean_auth with
    | None ->
      Dream.json ~status:`Unauthorized
        {|{"errcode": "M_UNAUTHORIZED", "error": "Missing Authorization header"}|}
    | Some (origin, key_id, signature) -> (
      let%lwt tree = Store.tree t.store in
      (* fetch the server's key *)
      let%lwt key_tree =
        Store.Tree.find_tree tree @@ Store.Key.v ["keys"; origin; key_id] in
      let%lwt key_tree =
        if key_tree = None then
          let%lwt key_s = Helper.fetching_key t origin key_id in
          match key_s with
          | Some (key_s, valid_until) -> (
            let%lwt tree =
              Store.Tree.add tree
                (Store.Key.v ["keys"; origin; key_id; "key"])
                key_s in
            let%lwt tree =
              Store.Tree.add tree
                (Store.Key.v ["keys"; origin; key_id; "valid_until"])
                (Int.to_string valid_until) in
            let%lwt return =
              Store.set_tree
                ~info:(Helper.info t ~message:"add server key")
                t.store (Store.Key.v []) tree in
            match return with
            | Ok () ->
              Store.Tree.find_tree tree @@ Store.Key.v ["keys"; origin; key_id]
            | Error write_error ->
              Dream.error (fun m ->
                  m "Write error: %a"
                    (Irmin.Type.pp Store.write_error_t)
                    write_error);
              Lwt.return_none)
          | None -> Lwt.return_none
        else Lwt.return key_tree in
      match key_tree with
      | None -> unkown_key key_id
      | Some key_tree ->
        let%lwt is_valid = Helper.is_valid_key key_tree in
        if not is_valid then unkown_key key_id
        else
          let%lwt key = Store.Tree.get key_tree (Store.Key.v ["key"]) in
          let%lwt request, is_valid =
            is_valid_signature origin t.server_name signature key request in
          if not is_valid then unkown_key key_id
          else handler (Dream.with_local logged_server origin request))

  module Rate_limit = struct
    let rate_limit_table = Hashtbl.create ~random:true 10

    (** The error code is getting old, it needs to be rewritten with all the new
      methods*)
    let limit_exceeded wait =
      Dream.json ~status:`Too_Many_Requests
        (Fmt.str
           {|{"errcode": "M_LIMIT_EXCEEDED", "error": "Rate limit exceed, please wait before retrying", "retry_after_ms": %d}|}
           (Float.to_int wait))

    (** [rate_limited ?(rate_limit) ?max ?delay handler request] is a middleware
      which goal is to rate-limit users when they make too much requests in a
      given time period.

      {ul
      {- [rate_limit] is the hash table used in by the function. It may be
        specified by hand if you want some routes to have a separate
        rate-limitation.}
      {- [max] is the maximum number of requests that can be made within the
        authorised delay. It's default value is 10. If specified, it has to be
        greater than 0.}
      {- [delay] is the delay used when checking if too much requests were made
      in a given time. It's default value is 3 seconds. If specified, it should
      always be positive.}
      {- [handler] and [request] are the usual arguments of a middleware.}}
      *)
    let rate_limited
        ?rate_limit_table:(table = rate_limit_table)
        ?(max = 3)
        ?(delay = 3000.)
        handler
        request =
      let client = Dream.client request in
      let client =
        match Ipaddr.of_string client with
        | Ok t -> Ipaddr.to_domain_name t |> Domain_name.to_string
        | Error _ -> client in
      let i, a =
        match Hashtbl.find_opt table client with
        | Some t -> t
        | None -> 0, Array.make max 0. in
      let time = Unix.time () *. 1000. in
      let old_time = a.(i) in
      if time > old_time +. delay then (
        a.(i) <- time;
        Hashtbl.replace table client ((i + 1) mod Array.length a, a);
        handler request)
      else limit_exceeded (old_time +. delay -. time)
  end
end
