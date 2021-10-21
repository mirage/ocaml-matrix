open Routing

let public_rooms =
  meths
    [
      `GET, Federation_endpoints.Listing.Public_rooms.get;
      `POST, Federation_endpoints.Listing.Public_rooms.post;
    ]

let make_join =
  variable (variable (meths [`GET, Federation_endpoints.Join.get]))

let send_join_v1 =
  variable (variable (meths [`PUT, Federation_endpoints.Join.put_v1]))

let send_join_v2 =
  variable (variable (meths [`PUT, Federation_endpoints.Join.put_v2]))

let v1 =
  paths
    [
      "publicRooms", public_rooms; "make_join", make_join;
      "send_join", send_join_v1;
    ]

let v2 = paths ["send_join", send_join_v2]

let direct_keys =
  node ~variable:(meths [`GET, Federation_endpoints.Keys.Server.get_direct]) ()

let indirect_keys =
  node
    ~meths:[`POST, Federation_endpoints.Keys.Server.get_all_indirect]
    ~variable:(variable (meths [`GET, Endpoint.placeholder]))
    ()

let key = paths ["v2", paths ["server", direct_keys; "query", indirect_keys]]
let matrix = paths ["federation", paths ["v1", v1; "v2", v2]; "key", key]
let routes : unit t = paths ["", paths ["_matrix", matrix]]
