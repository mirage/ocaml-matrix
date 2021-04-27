open Cohttp

let response_style ppf status =
  match status with
  | `Code _ -> Fmt.pf ppf "%s" (Cohttp.Code.string_of_status status)
  | #Code.informational_status ->
    Fmt.pf ppf "%a"
      Fmt.(styled `Blue (styled `Bold string))
      (Cohttp.Code.string_of_status status)
  | #Code.success_status ->
    Fmt.pf ppf "%a"
      Fmt.(styled `Green (styled `Bold string))
      (Cohttp.Code.string_of_status status)
  | #Code.redirection_status ->
    Fmt.pf ppf "%a"
      Fmt.(styled `Cyan (styled `Bold string))
      (Cohttp.Code.string_of_status status)
  | #Code.client_error_status ->
    Fmt.pf ppf "%a"
      Fmt.(styled `Red (styled `Bold string))
      (Cohttp.Code.string_of_status status)
  | #Code.server_error_status ->
    Fmt.pf ppf "%a"
      Fmt.(styled `Magenta (styled `Bold string))
      (Cohttp.Code.string_of_status status)

let make_headers
    ?(meths = [`GET; `POST; `PUT; `DELETE; `OPTIONS])
    ?(body_type = "application/json")
    () =
  if body_type = "text/html" then
    let headers = Header.init () in
    headers
  else
    let headers = Header.init () in
    let headers = Header.add headers "Content-Type" body_type in
    let headers = Header.add headers "Access-Control-Allow-Origin" "*" in
    let meths = List.map Code.string_of_method meths in
    let meths =
      Fmt.to_to_string (Fmt.list ~sep:(Fmt.any ",") Fmt.string) meths in
    let headers = Header.add headers "Access-Control-Allow-Methods" meths in
    let headers =
      Header.add headers "Access-Control-Allow-Headers"
        "Origin, X-Requested-With, Content-Type, Accept, Authorization" in
    headers
