let ( <.> ) f g x = f (g x)
let src = Logs.Src.create "paf-cohttp"

module Log = (val Logs.src_log src : Logs.LOG)

let scheme = Mimic.make ~name:"paf-scheme"
let port = Mimic.make ~name:"paf-port"
let domain_name = Mimic.make ~name:"paf-domain-name"
let ipaddr = Mimic.make ~name:"paf-ipaddr"
let sleep = Mimic.make ~name:"paf-sleep"

type ctx = Mimic.ctx

let default_ctx = Mimic.empty
let httpaf_config = Mimic.make ~name:"httpaf-config"
let error_handler mvar err = Lwt.async @@ fun () -> Lwt_mvar.put mvar err

let response_handler mvar pusher resp body =
  let on_eof () = pusher None in
  let rec on_read buf ~off ~len =
    let str = Bigstringaf.substring buf ~off ~len in
    pusher (Some str);
    Dream_httpaf.Body.Reader.schedule_read ~on_eof ~on_read body in
  Dream_httpaf.Body.Reader.schedule_read ~on_eof ~on_read body;
  Lwt.async @@ fun () -> Lwt_mvar.put mvar resp

let rec unroll body stream =
  let open Lwt.Infix in
  Lwt_stream.get stream >>= function
  | Some str ->
    Log.debug (fun m -> m "Transmit to HTTP/AF: %S." str);
    Dream_httpaf.Body.Writer.write_string body str;
    unroll body stream
  | None ->
    Log.debug (fun m -> m "Close the HTTP/AF writer.");
    Dream_httpaf.Body.Writer.close body;
    Lwt.return_unit

let transmit cohttp_body httpaf_body =
  match cohttp_body with
  | `Empty -> Dream_httpaf.Body.Writer.close httpaf_body
  | `String str ->
    Dream_httpaf.Body.Writer.write_string httpaf_body str;
    Dream_httpaf.Body.Writer.close httpaf_body
  | `Strings sstr ->
    List.iter (Dream_httpaf.Body.Writer.write_string httpaf_body) sstr;
    Dream_httpaf.Body.Writer.close httpaf_body
  | `Stream stream -> Lwt.async @@ fun () -> unroll httpaf_body stream

exception Internal_server_error
exception Invalid_response_body_length of Dream_httpaf.Response.t
exception Malformed_response of string

let with_uri uri ctx =
  let scheme_v =
    match Uri.scheme uri with
    | Some "http" -> Some `HTTP
    | Some "https" -> Some `HTTPS
    | _ -> None in
  let port_v =
    match Uri.port uri, scheme_v with
    | Some port, _ -> Some port
    | None, Some `HTTP -> Some 80
    | None, Some `HTTPS -> Some 443
    | _ -> None in
  let domain_name_v, ipaddr_v =
    match Uri.host uri with
    | Some v -> (
      match
        ( Result.bind (Domain_name.of_string v) Domain_name.host,
          Ipaddr.of_string v )
      with
      | _, Ok v -> None, Some v
      | Ok v, _ -> Some v, None
      | _ -> None, None)
    | _ -> None, None in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add scheme v ctx) scheme_v in
  let ctx = Option.fold ~none:ctx ~some:(fun v -> Mimic.add port v ctx) port_v in
  let ctx =
    Option.fold ~none:ctx ~some:(fun v -> Mimic.add ipaddr v ctx) ipaddr_v in
  let ctx =
    Option.fold ~none:ctx
      ~some:(fun v -> Mimic.add domain_name v ctx)
      domain_name_v in
  ctx

let with_host headers uri =
  let hostname = Uri.host_with_default ~default:"localhost" uri in
  let hostname =
    match Uri.port uri with
    | Some port -> Fmt.str "%s:%d" hostname port
    | None -> hostname in
  Dream_httpaf.Headers.add_unless_exists headers "host" hostname

let with_transfer_encoding ~chunked (meth : Cohttp.Code.meth) body headers =
  match
    meth, chunked, body, Dream_httpaf.Headers.get headers "content-length"
  with
  | `GET, _, _, _ -> headers
  | _, (None | Some false), _, Some _ -> headers
  | _, Some true, _, (Some _ | None) | _, None, `Stream _, None ->
    (* XXX(dinosaure): I'm not sure that the [Some _] was right. *)
    Dream_httpaf.Headers.add_unless_exists headers "transfer-encoding" "chunked"
  | _, (None | Some false), `Empty, None ->
    Dream_httpaf.Headers.add_unless_exists headers "content-length" "0"
  | _, (None | Some false), `String str, None ->
    Dream_httpaf.Headers.add_unless_exists headers "content-length"
      (string_of_int (String.length str))
  | _, (None | Some false), `Strings sstr, None ->
    let len = List.fold_right (( + ) <.> String.length) sstr 0 in
    Dream_httpaf.Headers.add_unless_exists headers "content-length"
      (string_of_int len)
  | _, Some false, `Stream _, None ->
    invalid_arg "Impossible to transfer a stream with a content-length value"

module Dream_httpaf_Client_connection = struct
  include Dream_httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

let call
    ?(ctx = default_ctx)
    ?headers
    ?body:(cohttp_body = Cohttp_lwt.Body.empty)
    ?chunked
    meth
    uri =
  Log.debug (fun m -> m "Fill the context with %a." Uri.pp uri);
  let ctx = with_uri uri ctx in
  let config =
    match Mimic.get httpaf_config ctx with
    | Some config -> config
    | None -> Dream_httpaf.Config.default in
  let sleep =
    match Mimic.get sleep ctx with
    | Some sleep -> sleep
    | None -> fun _ -> Lwt.return_unit
    (* TODO *) in
  let headers =
    match headers with
    | Some headers ->
      Dream_httpaf.Headers.of_list (Cohttp.Header.to_list headers)
    | None -> Dream_httpaf.Headers.empty in
  let headers = with_host headers uri in
  let headers = with_transfer_encoding ~chunked meth cohttp_body headers in
  let meth =
    match meth with
    | #Dream_httpaf.Method.t as meth -> meth
    | #Cohttp.Code.meth as meth -> `Other (Cohttp.Code.string_of_method meth)
  in
  let req = Dream_httpaf.Request.create ~headers meth (Uri.path_and_query uri) in
  let stream, pusher = Lwt_stream.create () in
  let mvar_res = Lwt_mvar.create_empty () in
  let mvar_err = Lwt_mvar.create_empty () in
  let open Lwt.Infix in
  Mimic.resolve ctx >>= function
  | Error (#Mimic.error as err) ->
    Lwt.fail (Failure (Fmt.str "%a" Mimic.pp_error err))
  | Ok flow -> (
    let error_handler = error_handler mvar_err in
    let response_handler = response_handler mvar_res pusher in
    let conn = Dream_httpaf.Client_connection.create ~config in
    let httpaf_body =
      Dream_httpaf.Client_connection.request conn ~error_handler
        ~response_handler req in
    Lwt.async (fun () ->
        Dream_paf.run ~sleep (module Dream_httpaf_Client_connection) conn flow);
    transmit cohttp_body httpaf_body;
    Log.debug (fun m -> m "Body transmitted.");
    Lwt.pick
      [
        (Lwt_mvar.take mvar_res >|= fun res -> `Response res);
        (Lwt_mvar.take mvar_err >|= fun err -> `Error err);
      ]
    >>= function
    | `Error (`Exn exn) -> Mimic.close flow >>= fun () -> Lwt.fail exn
    | `Error (`Invalid_response_body_length resp) ->
      Mimic.close flow >>= fun () ->
      Lwt.fail (Invalid_response_body_length resp)
    | `Error (`Malformed_response err) ->
      Mimic.close flow >>= fun () -> Lwt.fail (Malformed_response err)
    | `Response resp ->
      Log.debug (fun m -> m "Response received.");
      let version =
        match resp.Dream_httpaf.Response.version with
        | {Dream_httpaf.Version.major= 1; minor= 0} -> `HTTP_1_0
        | {major= 1; minor= 1} -> `HTTP_1_1
        | {major; minor} -> `Other (Fmt.str "%d.%d" major minor) in
      let status =
        match
          (resp.Dream_httpaf.Response.status
            :> [ Cohttp.Code.status | Dream_httpaf.Status.t ])
        with
        | #Cohttp.Code.status as status -> status
        | #Dream_httpaf.Status.t as status ->
          `Code (Dream_httpaf.Status.to_code status) in
      let encoding =
        match meth with
        | #Dream_httpaf.Method.standard as meth -> (
          match Dream_httpaf.Response.body_length ~request_method:meth resp with
          | `Chunked | `Close_delimited -> Cohttp.Transfer.Chunked
          | `Error _err -> raise Internal_server_error
          | `Fixed length -> Cohttp.Transfer.Fixed length)
        | _ -> Cohttp.Transfer.Chunked in
      let headers =
        Cohttp.Header.of_list
          (Dream_httpaf.Headers.to_list resp.Dream_httpaf.Response.headers)
      in
      let resp = Cohttp.Response.make ~version ~status ~encoding ~headers () in
      Lwt.return (resp, `Stream stream))

open Lwt.Infix

let head ?ctx ?headers uri = call ?ctx ?headers `HEAD uri >|= fst
let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

let delete ?ctx ?body ?chunked ?headers uri =
  call ?ctx ?body ?chunked ?headers `DELETE uri

let post ?ctx ?body ?chunked ?headers uri =
  call ?ctx ?body ?chunked ?headers `POST uri

let put ?ctx ?body ?chunked ?headers uri =
  call ?ctx ?body ?chunked ?headers `PUT uri

let patch ?ctx ?body ?chunked ?headers uri =
  call ?ctx ?body ?chunked ?headers `PATCH uri

let post_form ?ctx:_ ?headers:_ ~params:_ _uri = assert false (* TODO *)

let callv ?ctx:_ _uri _stream = assert false (* TODO *)

[@@@warning "-32"]

let sexp_of_ctx _ctx = assert false

[@@@warning "+32"]
