open Lwt
open Cohttp
open Cohttp_lwt_unix
open Json_encoding

exception Json_error of Matrix_ctos.Errors.t

let auth_token = ref ""

let make_uri path query =
  Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port:8008 ~path ~query ()

let make_headers ?(header = []) needs_auth =
  let headers = Header.of_list header in
  let headers =
    if needs_auth then
      Header.add headers "Authorization" ("Bearer " ^ !auth_token)
    else headers in
  headers

let make_federation_headers origin key sign ?(header = []) needs_auth =
  let headers = Header.of_list header in
  let auth =
    Fmt.str "X-Matrix origin=%s,key=\"%s\",sig=\"%s\"" origin key sign in
  let headers =
    if needs_auth then Header.add headers "Authorization" auth else headers
  in
  headers

let process response_encoding (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Fmt.(pf stdout "Response code: %d\n" code)
  ; body |> Cohttp_lwt.Body.to_string >|= fun body ->
    print_endline ("Received body\n" ^ body)
    ; let json_body = Ezjsonm.from_string body in
      if code >= 400 then
        raise (Json_error (destruct Matrix_ctos.Errors.encoding json_body))
      else destruct response_encoding json_body

let get
    ?(make_headers = make_headers)
    ?header
    path
    args
    response_encoding
    needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Get")
  ; let uri = make_uri path args in
    Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri)
    ; let headers = make_headers ?header needs_auth in
      Client.get ~headers uri >>= process response_encoding

let post ?(make_headers = make_headers) ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post")
  ; let uri = make_uri path args in
    Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri)
    ; let body = construct request_encoding value |> Ezjsonm.value_to_string in
      Fmt.(pf stdout "Sending body: %s\n%!" body)
      ; let body = Cohttp_lwt.Body.of_string body in
        let headers = make_headers ?header needs_auth in
        Client.post ~headers ~body uri >>= process response_encoding

let put ?(make_headers = make_headers) ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post")
  ; let uri = make_uri path args in
    Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri)
    ; let body = construct request_encoding value |> Ezjsonm.value_to_string in
      Fmt.(pf stdout "Sending body: %s\n%!" body)
      ; let body = Cohttp_lwt.Body.of_string body in
        let headers = make_headers ?header needs_auth in
        Client.put ~headers ~body uri >>= process response_encoding
