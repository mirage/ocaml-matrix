open Matrix_ctos
open Json_encoding
open Store
open Lwt.Infix
open Helpers

(** This needs a lot of work:
    - handling the headers in both ways,
    - working proper thumbnail
    - security issues which are not to be underestimated:
      https://matrix.org/docs/spec/client_server/latest#id408 might be a good
      start
    - need to isolate files between users
    - check upload size when downloading too *)

let upload =
  let open Media.Upload in
  let f () request query _ = (* maybe do something with the filename *)
    if String.length request > Const.upload_size
    then
      Lwt.return (`Request_entity_too_large, error "M_TOO_LARGE" (Fmt.str "Cannot upload files larger than %a" Fmt.byte_size Const.upload_size), None)
    else
      let id = id () in
      let filename = List.assoc_opt "filename" query |> (Option.map List.hd) in
      (match filename with
      | None -> Lwt.return_ok ()
      | Some filename ->
        Store.set store Key.(v "media" / "filenames" / filename) id >>=
        (function
          | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
          | Ok () -> Lwt.return_ok ())) >>=
          (function
          | Error err -> Lwt.return err
          | Ok () ->
            Store.set store Key.(v "media" / "files" / id) request >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
              | Ok () ->
                let response =
                  Response.make
                    ~content_uri:("mxc://" ^ Const.homeserver ^ "/" ^ id)
                    ()
                in
                let response =
                  construct Response.encoding response |>
                  Ezjsonm.value_to_string
                in
                Lwt.return (`OK, response, None)))
  in
  needs_auth, f

let download =
  let open Media.Download in
  let f (((), _server_name), media_id) _ _ _ =
    Store.get store Key.(v "media" / "files" / media_id) >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok media ->
        Lwt.return (`OK, media, None))
  in
  needs_auth, f

let download_filename = (* It's kind of ambiguous what this endpoint should do *)
  let open Media.Download_filename in
  let f ((((), _server_name), media_id), _filename) _ _ _ =
    Store.get store Key.(v "media" / "files" / media_id) >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok media ->
        Lwt.return (`OK, media, None))
  in
  needs_auth, f

let thumbnail =
  let open Media.Thumbnail in
  let f (((), _server_name), media_id) _ _ _ = (* Totally a shameful placeholder *)
    Store.get store Key.(v "media" / "files" / media_id) >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok media ->
        Lwt.return (`OK, media, None))
  in
  needs_auth, f

let config =
  let open Media.Config in
  let f () _ _ _ =
    let response =
      Response.make
        ~upload_size:Const.upload_size
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response, None)
  in
  needs_auth, f
