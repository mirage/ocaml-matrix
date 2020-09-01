open Json_encoding

type t =
  | Room_event of Room_events.t
  | Message_event of Message_event.t
  | State_event of State_events.t

let encoding =
  union
    [ case Room_events.encoding (function Room_event t -> Some t | _ -> None) (fun t -> Room_event t)
    ; case Message_event.encoding (function Message_event t -> Some t | _ -> None) (fun t -> Message_event t)
    ; case State_events.encoding (function State_event t -> Some t | _ -> None) (fun t -> State_event t)]
