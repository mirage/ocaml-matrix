open Lwt
open Cohttp
open Cohttp_lwt_unix
open Matrix.Errors
open Json_encoding

exception Json_error of t

let auth_token = ref ""

let make_uri path query =
  Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port:8123 ~path ~query ()

let make_headers ?(header=[]) needs_auth =
  let headers = Header.of_list header in
  let headers =
    if needs_auth
    then
      Header.add headers "Authorization" ("Bearer " ^ !auth_token)
    else
      headers
  in
  headers

let process response_encoding (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Fmt.(pf stdout "Response code: %d\n" code);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  print_endline ("Receivedw body\n" ^ body);
  let f = open_out "body.json" in
  output_string f body;
  close_out f;
  let json_body = Ezjsonm.from_string body in
  if code >= 400
  then
    raise (Json_error (destruct encoding json_body))
  else
    destruct response_encoding json_body

let get_j ?header path args response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Get");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.get ~headers uri >>= (process response_encoding)

let post_j_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.post ~headers ~body uri >>= (process response_encoding)

let put_j_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Put");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.put ~headers ~body uri >>= (process response_encoding)

let delete_j_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Delete");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.delete ~headers ~body uri >>= (process response_encoding)

let process_raw ?(header=false) response_encoding (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Fmt.(pf stdout "Response code: %d\n" code);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  print_endline ("Received body\n" ^ body);
  if header
  then
    print_endline ("Received headers\n" ^ (Response.headers resp |> Header.to_string));
  let f = open_out "body.txt" in
  output_string f body;
  close_out f;
  response_encoding body

let options ?header path args response_encoding =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Options");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let headers = make_headers ?header false in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.call ~headers `OPTIONS uri >>= (process_raw ~header:true response_encoding)

let get_r ?header path args response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Get");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.get ~headers uri >>= (process_raw response_encoding)

let post_j_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.post ~headers ~body uri >>= (process_raw response_encoding)

let put_j_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Put");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.put ~headers ~body uri >>= (process_raw response_encoding)

let delete_j_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Delete");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = construct request_encoding value |> Ezjsonm.value_to_string in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.delete ~headers ~body uri >>= (process_raw response_encoding)

let post_r_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.post ~headers ~body uri >>= (process response_encoding)

let put_r_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Put");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.put ~headers ~body uri >>= (process response_encoding)

let delete_r_j ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Delete");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.delete ~headers ~body uri >>= (process response_encoding)

let post_r_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Post");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.post ~headers ~body uri >>= (process_raw response_encoding)

let put_r_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Put");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.put ~headers ~body uri >>= (process_raw response_encoding)

let delete_r_r ?header path args value request_encoding response_encoding needs_auth =
  Fmt.(pf stdout "Request type: %a\n%!" (styled `Magenta string) "Delete");
  let uri = make_uri path args in
  Fmt.(pf stdout "Reaching query: %a\n%!" Uri.pp uri);
  let body = request_encoding value in
  Fmt.(pf stdout "Sending body: %s\n%!" body);
  let body = Cohttp_lwt.Body.of_string body in
  let headers = make_headers ?header needs_auth in
  (* Fmt.(pf stdout "Query headers: %a\n%!" Header.pp_hum headers); *)
  Client.delete ~headers ~body uri >>= (process_raw response_encoding)

let get = get_j

let post = post_j_j

let put = put_j_j

let delete = delete_j_j
