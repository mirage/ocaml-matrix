open Cohttp

type endpoint =
  string ->
  (string * string list) list ->
  string option ->
  (Code.status_code * string * string option) Lwt.t

type 'a path = string * 'a t

and 'a meth = Code.meth * (bool * ('a -> endpoint))

and 'a variable = ('a * string) t

and 'a t = Node of 'a meth list * 'a path list * 'a variable option

let parse uri meth routes =
  let paths = String.split_on_char '/' uri in
  let rec find : type k. k t -> k -> string list -> (bool * endpoint) option =
    function
    | Node (meths, paths, variable) -> (
      fun acc p ->
        match p with
        | [] -> Option.map (fun (b, f) -> b, f acc) (List.assoc_opt meth meths)
        | x :: t -> (
          match List.assoc_opt x paths with
          | Some node -> find node acc t
          | None ->
            Option.map (fun node -> find node (acc, x) t) variable
            |> Option.join)) in
  find routes () paths

let option uri routes =
  let paths = String.split_on_char '/' uri in
  let rec find : type k. k t -> k -> string list -> Code.meth list option =
    function
    | Node (meths, paths, variable) -> (
      fun acc p ->
        match p with
        | [] -> Some (List.map fst meths)
        | x :: t -> (
          match List.assoc_opt x paths with
          | Some node -> find node acc t
          | None ->
            Option.map (fun node -> find node (acc, x) t) variable
            |> Option.join)) in
  find routes () paths

let node ?(paths = []) ?(meths = []) ?variable () = Node (meths, paths, variable)
let paths paths = Node ([], paths, None)
let meths meths = Node (meths, [], None)
let variable variable = Node ([], [], Some variable)
