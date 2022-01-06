type context

val context :
  ?max_connections:int ->
  host:string ->
  port:int ->
  scheme:[ `Http | `Https ] ->
  user:string ->
  pwd:string ->
  device:string option ->
  unit ->
  context
(** [post ~host ~port ~scheme ~user ~password ?device] makes a context for a minimalist matrix
    client.
    [host] is the server to be contacted with port [port] using http or https, while [user] and
    [pwd] are the credentials used for the login.
    The optionnal [device] represents the key that will be associated with the
    future transactions for per device operations by the matrix server
    (disconnection of a given device, blocking, etc.) *)

val cmdliner : context option Cmdliner.Term.t

module Room : sig
  type t

  val make :
    context ->
    alias:string Current.t ->
    ?name:string Current.t ->
    ?topic:string Current.t ->
    ?power_level_content_override:
      Matrix_common.Events.Event_content.Power_levels.t Current.t ->
    unit ->
    t Current.t
  (** [make context ~alias ~name ~topic ()] manages a room accessible using [alias], 
      with given [name] and [topic]. It notably makes sure that the room exist and 
      that it has the chosen name and topic. The user needs rights to create rooms, 
      and errors might occur if the user don't have rights for a room it needs access 
      to.*)
end

type message = Matrix_common.Events.Event_content.Message.t

val post :
  context ->
  key:string Current.t ->
  room:Room.t Current.t ->
  message Current.t ->
  unit Current.t
(** [post context ~key ~room message] records that [key] is now set to [message], and
    uses a minimalist matrix client to send [message] to [room] using the user
    given in [context]. *)

module Raw = Client
