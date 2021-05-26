open Json_encoding
open Matrix_common

module Dummy = struct
  type t = unit [@@deriving accessor]

  let encoding = unit
  let pp ppf () = Fmt.(pf ppf "{ }")
end

module Password = struct
  module V1 = struct
    type t = {user: string; password: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.user, t.password in
      let of_tuple v =
        let user, password = v in
        {user; password} in
      let with_tuple = obj2 (req "user" string) (req "password" string) in
      conv to_tuple of_tuple with_tuple

    let pp ppf t = Fmt.(pf ppf "{ user: %s; password: %s }" t.user t.password)
  end

  module V2 = struct
    type t = {identifier: Identifier.t; password: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.identifier, t.password in
      let of_tuple v =
        let identifier, password = v in
        {identifier; password} in
      let with_tuple =
        obj2 (req "identifier" Identifier.encoding) (req "password" string)
      in
      conv to_tuple of_tuple with_tuple

    let pp ppf t =
      Fmt.(
        pf ppf "{ identifier: %a; password: %s }" Identifier.pp t.identifier
          t.password)
  end

  type t = V1 of V1.t | V2 of V2.t

  let encoding =
    union
      [
        case V1.encoding (function V1 t -> Some t | _ -> None) (fun t -> V1 t)
      ; case V2.encoding (function V2 t -> Some t | _ -> None) (fun t -> V2 t)
      ]

  let pp ppf = function
    | V1 t -> Fmt.(pf ppf "V1 of %a" V1.pp t)
    | V2 t -> Fmt.(pf ppf "V2 of %a" V2.pp t)
end

module Token = struct
  type t = {token: string; txn_id: string} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.token, t.txn_id in
    let of_tuple v =
      let token, txn_id = v in
      {token; txn_id} in
    let with_tuple = obj2 (req "token" string) (req "txn_id" string) in
    conv to_tuple of_tuple with_tuple

  let pp ppf t = Fmt.(pf ppf "{ token: %s; txn_id: %s }" t.token t.txn_id)
end

module Empty = Empty.Json

type t =
  | Dummy of Dummy.t
  | Password of Password.t
  | Token of Token.t
  | Empty of Empty.t

let encoding =
  let to_tuple t =
    let get_type = function
      | Dummy _ -> Some "m.login.dummy"
      | Password _ -> Some "m.login.password"
      | Token _ -> Some "m.login.token"
      | Empty _ -> None in
    get_type t, t in
  let of_tuple v =
    let _, t = v in
    t in
  let with_tuple =
    cond
      (obj1 (opt "type" string))
      [
        ( Some "m.login.dummy"
        , case Dummy.encoding
            (function Dummy t -> Some t | _ -> None)
            (fun t -> Dummy t) )
      ; ( Some "m.login.password"
        , case Password.encoding
            (function Password t -> Some t | _ -> None)
            (fun t -> Password t) )
      ; ( Some "m.login.token"
        , case Token.encoding
            (function Token t -> Some t | _ -> None)
            (fun t -> Token t) )
      ; ( None
        , case Empty.encoding
            (function Empty t -> Some t | _ -> None)
            (fun t -> Empty t) )
      ] in
  conv to_tuple of_tuple with_tuple

let pp ppf = function
  | Dummy t -> Fmt.(pf ppf "Dummy of %a" Dummy.pp t)
  | Password t -> Fmt.(pf ppf "Password of %a" Password.pp t)
  | Token t -> Fmt.(pf ppf "Token of %a" Token.pp t)
  | Empty t -> Fmt.(pf ppf "Empty of %a" Empty.pp t)
