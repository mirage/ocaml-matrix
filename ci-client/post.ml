open Lwt.Infix

type t = Client.t

let id = "matrix-post"

module Key = Current.String
module Value = Current.String
module Outcome = Current.Unit

let publish t job key message =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Job.log job "publishing message %S" message;
  Client.run job key t message >>= function
  | Ok () -> Lwt.return @@ Ok ()
  | Error s ->
    let msg = Fmt.str "Matrix post failed: %s" s in
    Lwt.return @@ Error (`Msg msg)

let pp f (key, value) = Fmt.pf f "Post %s: %s" key value
let auto_cancel = false
