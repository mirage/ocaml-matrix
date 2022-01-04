# Ocaml matrix

This project's goal is to have a server for the protocole matrix.
Be aware that it is a huge WIP and none of it should be considered definitive
nor finished

It is separated in several parts:
- json_encoding is a library used for the encoding of the endpoints in ocaml
  types. It heavily relies on the codebase of ocp-json-typed and should
  therefore be reworked in the near future
- lib contains three libraries:
  - matrix_ctos which is the encoding of the client to server endpoints (mostly
    done)
  - matrix_ctos which is the encoding of the client to server endpoints (mostly
    done, if not totally)
  - matrix_stos which is the encoding of the server to server endpoints (mostly
    done)

## CI client
This new part of the project which will be most likely separated in another
project in a close future is supposed to be a client for the ocaml CI:

It's goal is to be launched by the OCaml CI once a commit has been pushed to
the project: It will then connect to our matrix server as a pre-registered
client.

It then fetches all the room he is currently in and notices them that a new
commit was pushed with a message before disconnecting itself.

## CI server
The ci-server folder contains another implementation of the protocol, this time
of a minimalist server. Keep in mind that the server is heavy WIP, however the
bases of the project should be established.

This server is not ment to be used by regular users, only the CI client should
be used as it disables most of the endpoints and really regulates the
autorizations of the users inside it's rooms. Foreign users should use the
federation in order to get an access to the rooms of this server.

### Server setup

Before running the server, you will need an ed25519 key. You can generate such a
key with the following command:
```openssl genpkey -algorithm Ed25519 -out [key_path]```

Simply replace:
  - *key_path* with the future path to your ed25519 key pem file.

You will also need a store for your server database. Simply create a folder
where you want the store to be, run `git init` and create a commit. It is now
ready.

### Launching the server

```dune exec -- matrix-ci-server-bin [server_name] [key_name],[key_path] --store_path=[store_path]```

Simply replace:
  - *server_name* with the name of your server, it should be same that is
    defined in your dns entries.
  - *key_name* with the name given to your ed25519 key. It is use by the
    protocol in case a server would use several keys at a time (or renew it's
    keys).
  - *key_path* with the path to your ed25519 key pem file.
  - *store_path* with the path to your server store. This path is by default set
    to `/tmp/ocaml-matrix`

## Acknowledgement

`ocaml-matrix` has received funding from the Next Generation Internet Initiative (NGI) within the framework of the DAPSI Project.
