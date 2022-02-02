# Ocaml matrix

This project's goal is to have a server for the protocole matrix. Please, be
aware that it is a WIP

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
the project: It will then connect to the matrix server as a pre-registered
client.

It then fetches all the room he is currently in and notices them that a new
commit was pushed with a message before disconnecting itself.

## CI server
The ci-server folder contains another implementation of the protocol, this time
of a minimalist server. Keep in mind that the server is a WIP, however it
already supports the basics of a real server:

- For the client API, the various behaviors are supported:
  * login/logout
  * fetching of the available public rooms
  * room creation
  * reception of new state events and messages
All of the other endpoints are mostly disabled because the server was firstly
designed to be used by the CI client which does not need any other endpoints.

Indeed this server is not ment to be used by regular users, only the CI client
should be used as it disables most of the endpoints and really regulates the
autorizations of the users inside it's rooms. Foreign users should use the
federation in order to get an access to the rooms of this server.

- On the federation side, we implemented:
  * server key related endpoints such as fetching the public key of the server
  for authentication purposes.
  * fetching of the available public rooms
  * joining and leaving public rooms
  * backfilling of room's history
  * authentication of requests from foreign servers
  * updates to the foreign servers when updates are made to rooms
Those endpoints and mechanisms are enough to get a functionnal server trough the
federation. Some endpoints have been "softly" disabled (we answer with regular
errors) as some of them would always be forbidden anyway to any federation users.

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

## About the implementation

See [here](implem.md).

## State of the art

See [here](soa.md).

## Acknowledgement

`ocaml-matrix` has received funding from the Next Generation Internet Initiative (NGI) within the framework of the DAPSI Project.
