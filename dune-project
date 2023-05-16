(lang dune 3.7)
(name ocaml-matrix)
(license "ISC")
(authors "Gwenaëlle Lecat <charles-edouard@tarides.com>")
(maintainers "Gwenaëlle Lecat <charles-edouard@tarides.com>")
(source (github mirage/ocaml-matrix))
(documentation "https://mirage.github.io/ocaml-matrix")
(generate_opam_files true)
(formatting disabled)

(package
 (name matrix-ci-server)
 (synopsis "A minimalist matrix server for the hosting of CI trigerred messages")
 (description "A minimalist matrix server for the hosting of CI trigerred messages")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= 4.11))
  (matrix-ctos (= :version))
  (matrix-stos (= :version))
   dream-mirage
   (lwt_ppx (>= 2.1.0))
   astring
   base64
   bheap
   (cmdliner (>= 1.1.0))
   (ezjsonm (>= 1.3))
    fmt
    (irmin-fs (>= 3.0))
    (irmin-git (>= 3.6))
    logs
    (tcpip (>= 7.0.0))
    mirage-crypto-ec
    mirage-time
    mirage-time-unix
    mirage-clock-unix
    mirage-crypto-rng-mirage
    mirage-kv
    ca-certs-nss
    (ppxlib (>= 0.16.0))
    uuidm
    x509
    (shexp :with-test)))

(package
 (name matrix-common)
 (synopsis "Core library of the matrix spec")
 (description "Core library of the matrix spec")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= 4.11))
  (ezjsonm (>= 1.3))
   fmt
   logs
   (cmdliner (>= 1.1.0))
   (ppxlib (>= 0.16.0))))

(package
 (name matrix-ctos)
 (synopsis "Library for the client to server endpoints of matrix protocole")
 (description "Library for the client to server endpoints of matrix protocole")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= 4.11))
  (matrix-common (= :version))
  (ezjsonm (>= 1.3))
   fmt
   logs
  (ppxlib (>= 0.16.0))))

(package
 (name matrix-stos)
 (synopsis "Library for the server to server endpoints of matrix protocole")
 (description "Library for the server to server endpoints of matrix protocole")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= 4.11))
  (matrix-common (= :version))
  (ezjsonm (>= 1.3))
   fmt
   logs
   mirage-crypto-ec
   x509
  (ppxlib (>= 0.16.0))))

(package
 (name matrix-current)
 (synopsis "A minimalist matrix client for an ocurrent plugin")
 (description "A minimalist matrix client for an ocurrent plugin")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= 4.11))
  (matrix-ctos (= :version))
  (current (>= 0.6.4))
   cohttp
   cohttp-lwt
   cohttp-lwt-unix
  (ezjsonm (>= 1.3))
   fmt
   logs
   (lwt (>= 5.6.1))
 (ppxlib (>= 0.16.0))
  uuidm
  (ppx_deriving_yojson (>= 3.7.0))
  (shexp :with-test)))