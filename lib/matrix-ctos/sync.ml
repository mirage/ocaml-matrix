open Json_encoding
open Matrix_common

module Query = struct
  module Presence = struct
    type t = Offline | Online | Unavailable

    let to_string = function
      | Offline -> "offline"
      | Online -> "online"
      | Unavailable -> "unavailable"

    let pp ppf = function
      | Offline -> Fmt.pf ppf "Offline"
      | Online -> Fmt.pf ppf "Online"
      | Unavailable -> Fmt.pf ppf "Unavailable"
  end

  type t = {
      filter: string option
    ; since: string option
    ; full_state: bool option
    ; set_presence: Presence.t option
    ; timeout: int option
  }
  [@@deriving accessor]

  let args t =
    let l = [] in
    let l =
      match t.filter with None -> l | Some filter -> ("filter", [filter]) :: l
    in
    let l =
      match t.since with None -> l | Some since -> ("since", [since]) :: l in
    let l =
      match t.full_state with
      | None -> l
      | Some full_state ->
        let full_state = Bool.to_string full_state in
        ("full_state", [full_state]) :: l in
    let l =
      match t.set_presence with
      | None -> l
      | Some set_presence ->
        let set_presence = Presence.to_string set_presence in
        ("set_presence", [set_presence]) :: l in
    let l =
      match t.timeout with
      | None -> l
      | Some timeout ->
        let timeout = Int.to_string timeout in
        ("timeout", [timeout]) :: l in
    l

  let pp ppf t =
    Fmt.(
      pf ppf
        "{ filter: %a; since: %a; full_state: %a; set_presence: %a; timeout: \
         %a }"
        Dump.(option string)
        t.filter
        Dump.(option string)
        t.since
        Dump.(option bool)
        t.full_state
        Dump.(option Presence.pp)
        t.set_presence
        Dump.(option int)
        t.timeout)
end

module Response = struct
  type t = {
      next_batch: string
    ; rooms: Rooms.t option
    ; presence: Events.State_event.t list option
    ; account_data: Events.State_event.t list option
    ; to_device: Events.State_event.t list option
    ; device_lists: Device_lists.t option
    ; device_one_time_keys_count: (string * int) list option
    ; groups: Ezjsonm.value option (* Not on the documentation*)
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.next_batch
      , t.rooms
      , t.presence
      , t.account_data
      , t.to_device
      , t.device_lists
      , t.device_one_time_keys_count
      , t.groups ) in
    let of_tuple v =
      let ( next_batch
          , rooms
          , presence
          , account_data
          , to_device
          , device_lists
          , device_one_time_keys_count
          , groups ) =
        v in
      {
        next_batch
      ; rooms
      ; presence
      ; account_data
      ; to_device
      ; device_lists
      ; device_one_time_keys_count
      ; groups
      } in
    let with_tuple =
      obj8 (req "next_batch" string)
        (opt "rooms" Rooms.encoding)
        (opt "presence"
           (obj1 (req "events" (list Events.State_event.encoding))))
        (opt "account_data"
           (obj1 (req "events" (list Events.State_event.encoding))))
        (opt "to_device"
           (obj1 (req "events" (list Events.State_event.encoding))))
        (opt "device_lists" Device_lists.encoding)
        (opt "device_one_time_keys_count" (assoc int))
        (opt "groups" any) in
    conv to_tuple of_tuple with_tuple

  let pp ppf _t =
    (* PP TO DO *)
    Fmt.(pf ppf "TO DO")
  (* Fmt.(pf ppf "{ next_batch: %s; rooms: %a; presence: %a; account_data: %a; to_device: %a; device_lists: %a; device_one_time_keys_count: %a; groups }"
     t.next_batch
     Rooms.pp t.rooms
     Dump.(list Event.pp) t.presence
     Dump.(list Event.pp) t.account_data
     Dump.(list Event.pp) t.to_device
     Device_lists.pp t.device_lists
     Dump.(list (pair string int)) t.device_one_time_keys_count *)
end
