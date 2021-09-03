open Angstrom

module Server = struct
  type t =
    | IPv4 of Ipaddr.V4.t
    | IPv6 of Ipaddr.V6.t
    | Extension of string * string
    | Domain of string list

  let ( or ) a b x = a x || b x
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_dash = ( = ) '-'
  let let_dig = satisfy (is_alpha or is_digit)

  (* XXX(dinosaure): Ldh-str = *( ALPHA / DIGIT / "-" ) Let-dig
   * and Let-dig = ALPHA / DIGIT
   *
   * This [ldh_str] does not strictly follow the definition but just
   * eats [*( ALPHA / DIGIT / "-" )] and check that the last character
   * **is not** a dash. *)
  let ldh_str =
    take_while1 (is_alpha or is_digit or is_dash) >>= fun res ->
    if String.get res (String.length res - 1) <> '-' then return res
    else fail "Invalid ldh-str token"

  let sub_domain =
    let_dig >>= fun pre ->
    option "" ldh_str >>| fun lst -> String.concat "" [String.make 1 pre; lst]

  let domain =
    sub_domain >>= fun x ->
    many (char '.' *> sub_domain) >>| fun r -> Domain (x :: r)

  (* XXX(dinosaure): from mrmime. *)

  let is_dcontent = function
    | '\033' .. '\090' | '\094' .. '\126' -> true
    | _ -> false

  let ipv4_address_literal =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V4.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None)
    >>= function
    | Some v -> return v
    | None -> fail "ipv4_address_literal"

  let ipv6_addr =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V6.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None)
    >>= function
    | Some v -> return v
    | None -> fail "ipv6_addr"

  let ipv6_address_literal = string "IPv6:" *> ipv6_addr
  let failf fmt = Fmt.kstrf fail fmt

  let ldh_str =
    take_while1 (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
      | _ -> false)
    >>= fun ldh ->
    if String.unsafe_get ldh (String.length ldh - 1) = '-' then
      failf "ldh_str: %s is invalid" ldh
    else return ldh

  let general_address_literal =
    ldh_str <* char ':' >>= fun ldh ->
    take_while1 is_dcontent >>| fun value -> Extension (ldh, value)

  let address_literal =
    char '['
    *> (ipv4_address_literal
       >>| (fun v -> IPv4 v)
       <|> (ipv6_address_literal >>| fun v -> IPv6 v)
       <|> general_address_literal)
    <* char ']'

  let server = domain <|> address_literal

  let of_string_exn x =
    match parse_string ~consume:Consume.All server x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid domain: %s" x
end

module Room_alias = struct
  let is_sep = function ':' -> true | _ -> false
  let room_local = take_till is_sep
  let room_alias = ( and+ ) (char '#' *> room_local) (char ':' *> Server.server)

  let of_string_exn x =
    match parse_string ~consume:Consume.All room_alias x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid room alias: %s" x
end
