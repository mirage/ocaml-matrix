type context

val context : host:string -> port:int -> user:string -> pwd:string -> device:string option -> context
(** [post ~host ~port ~user ~password ?device] makes a context for a minimalist matrix
    client.
    [host] is the server to be contacted with port [port], while [user] and
    [pwd] are the credentials used for the login.
    The optionnal [device] represents the key that will be associated with the
    future transactions for per device operations by the matrix server
    (disconnection of a given device, blocking, etc.) *)

val post : context -> key:string -> string Current.t -> unit Current.t
(** [post context ~key message] records that [key] is now set to [message], and
    uses a minimalist matrix client to send [message] to every room the user
    given in [context] is part of. *)
