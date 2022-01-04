let user = "bonjour"
let pass = "bonjour"

let (let*) = Shexp_process.Let_syntax.(>>=)
let (let+) = Shexp_process.Let_syntax.(>>|)

let () =
let open Shexp_process in
  eval @@
  let* () = rm_rf "/tmp/ocaml-matrix/" in
  let* () = mkdir "/tmp/ocaml-matrix/" in
  let* () = chdir "/tmp/ocaml-matrix" (run "git" ["init"]) in
  let+ () = run "matrix-ci-server-setup" ["user"; "/tmp/ocaml-matrix/"; user; pass] in
  ()