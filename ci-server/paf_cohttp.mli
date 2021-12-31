val scheme : [ `HTTP | `HTTPS ] Mimic.value
val port : int Mimic.value
val domain_name : [ `host ] Domain_name.t Mimic.value
val ipaddr : Ipaddr.t Mimic.value
val sleep : (int64 -> unit Lwt.t) Mimic.value
val with_uri : Uri.t -> Mimic.ctx -> Mimic.ctx

include Cohttp_lwt.S.Client with type ctx = Mimic.ctx
