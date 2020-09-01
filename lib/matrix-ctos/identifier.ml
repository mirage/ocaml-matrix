open Json_encoding

module User =
struct
  type t =
    { user: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.user
    in
    let of_tuple v =
      let user = v in
      {user}
    in
    let with_tuple =
      obj1
        (req "user"
          string)
    in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(pf ppf "{ user: %s }"
      t.user)
end

module Thirdparty =
struct
  type t =
    { medium: string
    ; address: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.medium, t.address
    in
    let of_tuple v =
      let medium, address = v in
      {medium; address}
    in
    let with_tuple =
      obj2
        (req "medium"
          string)
        (req "address"
          string)
    in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(pf ppf "{ medium: %s ; address: %s }"
      t.medium
      t.address)
end

module Phone =
struct
  type t =
    { country: string
    ; phone: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.country, t.phone
    in
    let of_tuple v =
      let country, phone = v in
      {country; phone}
    in
    let with_tuple =
      obj2
        (req "country"
          string)
        (req "phone"
          string)
    in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(pf ppf "{ country: %s ; phone: %s }"
      t.country
      t.phone)
end

type t =
  | User of User.t
  | Thirdparty of Thirdparty.t
  | Phone of Phone.t

let encoding =
  let to_tuple t =
    let get_type = function
      | User _ -> "m.presence"
      | Thirdparty _ -> "m.push_rules"
      | Phone _ -> "m.typing"
    in
    get_type t, t
  in
  let of_tuple v =
    let _, t = v in t
  in
  let with_tuple =
    cond
      (obj1 (req "type" string))
      [ "m.id.user", case User.encoding (function User t -> Some t | _ -> None) (fun t -> User t)
      ; "m.id.thirdparty", case Thirdparty.encoding (function Thirdparty t -> Some t | _ -> None) (fun t -> Thirdparty t)
      ; "m.id.phone", case Phone.encoding (function Phone t -> Some t | _ -> None) (fun t -> Phone t) ]
  in
  conv to_tuple of_tuple with_tuple

let pp ppf = function
  | User t -> Fmt.(pf ppf "User of %a" User.pp t)
  | Thirdparty t -> Fmt.(pf ppf "Thirdparty of %a" Thirdparty.pp t)
  | Phone t -> Fmt.(pf ppf "Phone of %a" Phone.pp t)
