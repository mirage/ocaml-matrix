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
- fake_client is a fake client used to try out the endpoint's behaviours with
  synapse, the reference implementation
- server is the actual server, it is full of shortcuts and heavily wrong in many
  ways. Most of it's encoded features are only working in specific situations

In order to launch the server, you can use this command:

```dune exec -- ocaml-server --verbosity=debug```

By default, even with debug verbosity, I removed irmin, git and decompress logs
because they were generating too much noise. You can force their printing by
using the `-f` flag.
