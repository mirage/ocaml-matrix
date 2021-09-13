# Ocaml matrix

This project's goal is to have a server for the protocole matrix.
Be aware that it is a huge WIP and none of it should be considered definitive
nor finished

It is separated in several parts:
- json_encoding is a library used for the encoding of the endpoints in ocaml
  types. It heavily relies on the codebase of ocp-json-typed and should
  therefore be reworked in the near future
- lib contains two libraries:
  - matrix_ctos which is the encoding of the client to server endpoints (mostly
    done)
  - matrix_stos which is the encoding of the server to server endpoints (still
    in the beginning)
- server is the actual server, it is full of shortcuts and heavily wrong in many
  ways. Most of it's encoded features are only working in specific situations

In order to launch the server, you can use this command:

```dune exec -- matrix-server --verbosity=debug```

By default, even with debug verbosity, I removed irmin, git and decompress logs
because they were generating too much noise. You can force their printing by
using the `-f` flag.

## CI client
This new part of the project which will be most likely separated in another
project in a close future is supposed to be a client for the ocaml CI:

It's goal is to be launched by the OCaml CI once a commit has been pushed to
the project: It will then connect to our matrix server as a pre-registered
client.

It then fetches all the room he is currently in and notices them that a new
commit was pushed with a message before disconnecting itself.


## Acknowledgement 

`ocaml-matrix` has received funding from the Next Generation Internet Initiative (NGI) within the framework of the DAPSI Project.
