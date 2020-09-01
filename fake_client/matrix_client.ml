open Matrix
open Cmdliner
open Commands

let main () =
  try
    (* let _ = versions () in *)
    let _ = login gwen in
    let s = {|{"user_ids":["@gwenaelle:ocaml-matrix"]}|} in
    Fmt.pr "%s\n%!" (Lwt_main.run (Http.post_r_r "/_matrix/client/r0/publicised_groups" [] s (fun s -> s) (fun s -> s) true));
    (* let s = sync () in
    let _ = sync ~since:(Sync.Response.get_next_batch s) () in *)
    (* let _ = options "/_matrix/client/unstable/room_keys/version" in
    Fmt.pr "%s\n%!" (Lwt_main.run (Http.get_r "/_matrix/client/unstable/room_keys/version" [] (fun s -> s) true)); *)

    (* let s = {|{"one_time_keys":{"signed_curve25519:AAAABQ":{"key":"ribGjPE/EhFKJW6fW/aNE+V4OvzlhmOpCBJosuBALjk","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"SrBUZ9/aR1yF39ZDnh102j6uTNWoE6/nNmDd4XX68VwGEU6uUhvL8p+v5gBh3Sxegs4faJwVVUxUl9kN32lXCA"}}},"signed_curve25519:AAAABA":{"key":"5N2isofkFxhVmY64U9W1PTtACBf/aP0dmNxWYZEeP1Q","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"+CaoFJLiJUn5+1JARGe11BPTR5z8d0kywCS+3kYpM6PItlTdY9gv3zZrrVJPvEWyCHh8fP4t4Au293A0IzSmDQ"}}},"signed_curve25519:AAAAAw":{"key":"0B+1hQus3tTemqsOvZdpvVggBi7ttgFktjqHxNcxV1Q","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"R01P4Iu/fCFWN4AwdmqR1S7l1kyROZQ585FhMrcsBpkrANIrT3vMbvMF5hCsqOUBh35cvuWbWMeS9oIsuWVcCA"}}},"signed_curve25519:AAAAAg":{"key":"n2EFbPKK0d4NZTX38f28DesltdbBWqZQrZ+pts2/BQE","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"naNMPZPK06OZG2oASvwe6KMZBuihJNLuUvhBm+pPoPmQeYOarG5FpRns8rJfxojUle8UQLDczChla8NIU5XECw"}}},"signed_curve25519:AAAAAQ":{"key":"yfnK4lHAt4sh/KoTCbzgp7VeFrsBOpwdYAb0COSXC18","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"3OUslQMBEi22PPocGvggE+5W4GUm8jdKSMdI4Gofys1/yV3GyU4NXr1gsboGJApmtFGwzx2V481vSb4TkOCNCw"}}}}}|} in
    Fmt.pr "%s\n%!" (Lwt_main.run (Http.post_r_r "/_matrix/client/r0/keys/upload" [] s (fun s -> s) (fun s -> s) true));
    let s = {|{"one_time_keys":{"signed_curve25519:AAAABD":{"key":"ribGjPE/EhFKJW6fW/aNE+V4OvzlhmOpCBJosuBALjk","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"SrBUZ9/aR1yF39ZDnh102j6uTNWoE6/nNmDd4XX68VwGEU6uUhvL8p+v5gBh3Sxegs4faJwVVUxUl9kN32lXCA"}}},"signed_curve25519:AAAABA":{"key":"5N2isofkFxhVmY64U9W1PTtACBf/aP0dmNxWYZEeP1Q","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"+CaoFJLiJUn5+1JARGe11BPTR5z8d0kywCS+3kYpM6PItlTdY9gv3zZrrVJPvEWyCHh8fP4t4Au293A0IzSmDQ"}}},"signed_curve25519:AAAAAw":{"key":"0B+1hQus3tTemqsOvZdpvVggBi7ttgFktjqHxNcxV1Q","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"R01P4Iu/fCFWN4AwdmqR1S7l1kyROZQ585FhMrcsBpkrANIrT3vMbvMF5hCsqOUBh35cvuWbWMeS9oIsuWVcCA"}}},"signed_curve25519:AAAAAg":{"key":"n2EFbPKK0d4NZTX38f28DesltdbBWqZQrZ+pts2/BQE","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"naNMPZPK06OZG2oASvwe6KMZBuihJNLuUvhBm+pPoPmQeYOarG5FpRns8rJfxojUle8UQLDczChla8NIU5XECw"}}},"signed_curve25519:AAAAAQ":{"key":"yfnK4lHAt4sh/KoTCbzgp7VeFrsBOpwdYAb0COSXC18","signatures":{"@default:ocaml-matrix":{"ed25519:device_id":"3OUslQMBEi22PPocGvggE+5W4GUm8jdKSMdI4Gofys1/yV3GyU4NXr1gsboGJApmtFGwzx2V481vSb4TkOCNCw"}}}}}|} in
    Fmt.pr "%s\n%!" (Lwt_main.run (Http.post_r_r "/_matrix/client/r0/keys/upload" [] s (fun s -> s) (fun s -> s) true)); *)

    ()
  with
    | Http.Json_error t -> Fmt.(pf stdout "Encountered error: %a\n%!" (styled `Red (styled `Bold Errors.pp)) t)
    | e -> Fmt.(pf stdout "Json error: %a\n%!" (styled `Red (styled `Bold Json_encoding.print_error)) e)

let setup level =
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs.format_reporter ())

let () =
  let info =
    let doc = "poc of a matrix server" in
    Term.info "server" ~version:"%%VERSION%%" ~doc
  in
  Term.exit @@ Term.eval (Term.(const main $ Term.(const setup $ Logs_cli.level ())), info)
