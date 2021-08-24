open Server_utility
open Store
open Data

let logged_user =
  Dream.new_local ~name:"logged_user" ~show_value:(fun s -> s) ()

let logged_device =
  Dream.new_local ~name:"logged_device" ~show_value:(fun s -> s) ()

let clean_token token =
  let len = String.length token in
  let sub = String.sub token 0 (min len 7) in
  if String.equal sub "Bearer " then Some (String.sub token 7 (len - 7))
  else None

let unkown_token =
  Dream.json ~status:`Unauthorized
    {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched"}|}

(** Notes:
  - In cruel need of error handling
  - Check the token's validity
*)
let is_logged handler request =
  let token =
    match Option.bind (Dream.header "Authorization" request) clean_token with
    | None -> Dream.query "access_token" request
    | token -> token in
  match token with
  | None ->
    Dream.json ~status:`Unauthorized
      {|{"errcode": "M_MISSING_TOKEN", "error": "No access token was specified"}|}
  | Some token -> (
    (* fetch the token *)
    let%lwt s_token = Store.find store (Store.Key.v ["tokens"; token]) in
    match s_token with
    | None ->
      Dream.json ~status:`Unauthorized
        {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched tok"}|}
    | Some s_token -> (
      let s_token =
        Json_encoding.destruct Token.encoding
          (Ezjsonm.value_from_string s_token) in
      (* fetch the device *)
      let device = Token.get_device s_token in
      let%lwt s_device = Store.find store (Store.Key.v ["devices"; device]) in
      match s_device with
      | None ->
        Dream.json ~status:`Unauthorized
          {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched dev"}|}
      | Some s_device -> (
        let s_device =
          Json_encoding.destruct Device.encoding
            (Ezjsonm.value_from_string s_device) in
        (* fetch the user *)
        let user = Device.get_user s_device in
        let%lwt s_user = Store.find store (Store.Key.v ["users"; user]) in
        match s_user with
        | None ->
          Dream.json ~status:`Unauthorized
            {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched user"}|}
        | Some s_user ->
          let s_user =
            Json_encoding.destruct User.encoding
              (Ezjsonm.value_from_string s_user) in
          (* check the user/device/token *)
          if
            List.exists (String.equal device) (User.get_devices s_user)
            && String.equal token (Device.get_token s_device)
          then
            handler
              (Dream.with_local logged_device device
                 (Dream.with_local logged_user user request))
          else
            Dream.json ~status:`Unauthorized
              {|{"errcode": "M_UNKNOWN_TOKEN", "error": "No access token matched"}|}
        )))

let is_logged_server _ _ = assert false

module Rate_limit = struct
  let rate_limit =
    let show_value t =
      Fmt.str "%a"
        Fmt.(Dump.hashtbl string (Dump.pair int (Dump.array float)))
        t in
    Dream.new_global ~name:"rate_limit_table" ~show_value (fun () ->
        Hashtbl.create ~random:true 10)

  (** The error code is getting old, it needs to be rewritten with all the new
    methods*)
  let limit_exceeded wait =
    Dream.json ~status:`Unauthorized
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
      ?(rate_limit = rate_limit) ?(max = 3) ?(delay = 3000.) handler request =
    let table = Dream.global rate_limit request in
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
