open Lwt
open Cohttp
open Cohttp_lwt_unix
open Json_encoding

exception Json_error of string

module Server = struct
  type scheme = [ `Http | `Https ]

  let scheme_to_string = function `Http -> "http" | `Https -> "https"

  type t = {scheme: scheme; host: string; port: int}

  let pp f {scheme; host; port} =
    Fmt.pf f "%s://%s:%d" (scheme_to_string scheme) host port

  let v scheme host port = {scheme; host; port}

  let to_uri {scheme; host; port} path query =
    let scheme = scheme_to_string scheme in
    Uri.make ~scheme ~host ~port ~path ?query ()
end

let make_headers ?(header = []) auth_token =
  let headers = Header.of_list header in
  let headers =
    match auth_token with
    | None -> headers
    | Some auth_token ->
      Header.add headers "Authorization" (Fmt.str "Bearer %s" auth_token) in
  headers

type 'a or_error = ('a, Matrix_ctos.Errors.t) Result.t

let parse_or_fail code json_encoding body =
  let json_body = Ezjsonm.from_string body in
  if code >= 400 then Error (destruct Matrix_ctos.Errors.encoding json_body)
  else Ok (destruct json_encoding json_body)

let get server ?header path args response_encoding needs_auth =
  let uri = Server.to_uri server path args in
  let headers = make_headers ?header needs_auth in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  if body <> "" then parse_or_fail code response_encoding body
  else raise (Json_error (Fmt.str "Error %d in get: no body" code))

let post
    server ?header path args value request_encoding response_encoding auth_token
    =
  let uri = Server.to_uri server path args in
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header auth_token in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  if body <> "" then parse_or_fail code response_encoding body
  else raise (Json_error (Fmt.str "Error %d in post" code))

let put
    server ?header path args value request_encoding response_encoding auth_token
    =
  let uri = Server.to_uri server path args in
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header auth_token in
  Cohttp_lwt_unix.Client.put ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  if body <> "" then parse_or_fail code response_encoding body
  else raise (Json_error (Fmt.str "Error %d in put" code))
