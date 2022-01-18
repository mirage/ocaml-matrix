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
the project: It will then connect to the matrix server as a pre-registered
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

## About the implementation

The Matrix server manages clients who are registered to rooms where there exist events, which represent client actions such as sending a message. Our OCaml Matrix server implementation follows the Matrix specification standard from which we extract the parts describing the subset of Matrix components that we choose to implement for the MVP. The OCaml implementation environment provides secure by design properties and avoids various vulnerabilities such as the buffer overflow recently discovered to produce considerable information disclosure in other Matrix implementations such as Element. We note that Matrix maintains a hall of fame regarding the investigative work into Matrix security vulnerabilities.

The clients are user applications that connect to a Matrix server via the client-server API, which allows clients to perform actions such as sending messages, control rooms, or synchronize the conversation history. We remind that in Phase 1 we implemented an OCaml-CI client, which communicates with the Matrix unikernel via the client-server API provided here. We tested the integration of the OCaml-CI communication with both Synapse and the OCaml Matrix server. Regarding the GDPR requirements, we note that the Matrix unikernel supports a client authentication mechanism based on user name identification and password, according to the Matrix specification for authentication mechanisms.

The Matrix rooms define the notion of location where users can interact by sending and receiving events. The users that are present in a room will receive the events according to their access rights characteristics. The Matrix federation maintains room data structures representing the state of the room and messages. The state data comprises the (unique) room id, the list of servers accessing the room and the list of clients who are members to a room. The message data describes communication activities such as instant messages, video/audio call setups, or file transfers, which are implemented using secured channels. We note that we follow the Matrix specification for Room Version 6 (out of 7 versions). For now we implement in the OCaml Matrix server only access to public rooms, which does not require  the end-to-end encryption protocol. Nevertheless, we define support for encrypted communication via the *Key* module and we note that most of the encryption algorithms used by the end-to-end encryption protocol are available to be used in Mirage unikernels via the mirage-crypto library.

The events are sorted via two types of relations: topological and stream ordering. The topological ordering takes into account a timestamp related ordering where the most recent in time events are previous events candidates for the next event to be sent. The state of the room at a given timestamp is calculated by considering all events preceding the timestamp and a conflict resolution algorithm is used for data racing, when events apply to the same state of a room. Our implementation of the conflict resolution algorithm adapts the standard conflict resolution by reasoning upon a list structure instead of a directed acyclic graph as in the specification. This slight deviation from the specification is not relevant for the moment but we plan to rectify it and follow the specification to the letter. 

Setting the OCaml Matrix server into the unikernel format transforms it into a library operating system, which provides the infrastructure for the development of secure and high-performance code that is deployed as an operating system driver. The process of production deployment also ensures that the Matrix server unikernel is compatible with various platforms where the Matrix server can run in isolation, as such increasing the security level of the Matrix server. 

The unikernel format of the Matrix server is not completed and we plan this as the next step of work. We also consider as additional contexts for the future of the Matrix server unikernel to develop the feature of user access to private rooms with end-to-end encryption and more authentication methods that follow the Matrix specification and the GDPR recommendations.

## Acknowledgement

`ocaml-matrix` has received funding from the Next Generation Internet Initiative (NGI) within the framework of the DAPSI Project.
