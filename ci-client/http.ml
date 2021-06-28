open Lwt
open Cohttp
open Cohttp_lwt_unix
open Json_encoding

exception Json_error of string

let make_uri host port path query =
  Uri.make ~scheme:"http" ~host ~port ~path ?query ()

let make_headers ?(header = []) auth_token =
  let headers = Header.of_list header in
  let headers =
    match auth_token with
    | None -> headers
    | Some auth_token ->
      Header.add headers "Authorization" (Fmt.str "Bearer %s" auth_token) in
  headers

let get host port ?header path args response_encoding needs_auth =
  let uri = make_uri host port path args in
  let headers = make_headers ?header needs_auth in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let json_body = Ezjsonm.from_string body in
  if code >= 400 then
    raise (Json_error (Fmt.str "Json error in get: %a" Matrix_ctos.Errors.pp (destruct Matrix_ctos.Errors.encoding json_body)))
  else destruct response_encoding json_body

let post host port ?header path args value request_encoding response_encoding auth_token =
  let uri = make_uri host port path args in
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header auth_token in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let json_body = Ezjsonm.from_string body in
  if code >= 400 then
    raise (Json_error (Fmt.str "Json error in post: %a" Matrix_ctos.Errors.pp (destruct Matrix_ctos.Errors.encoding json_body)))
  else destruct response_encoding json_body

let put host port ?header path args value request_encoding response_encoding auth_token =
  let uri = make_uri host port path args in
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header auth_token in
  Cohttp_lwt_unix.Client.put ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let json_body = Ezjsonm.from_string body in
  if code >= 400 then
    raise (Json_error (Fmt.str "Json error in put: %s %a" path Matrix_ctos.Errors.pp (destruct Matrix_ctos.Errors.encoding json_body)))
  else destruct response_encoding json_body
