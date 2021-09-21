open Json_encoding

module Error = struct
  type t = {errcode: string; error: string option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.errcode, t.error in
    let of_tuple v =
      let errcode, error = v in
      {errcode; error} in
    let with_tuple = obj2 (req "errcode" string) (opt "error" string) in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(
      pf ppf "{errcode: %s; error: %a}" t.errcode (Dump.option string) t.error)
end

module Rate_limited = struct
  type t = {errcode: string; error: string; retry_after_ms: int option}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = (), t.error, t.retry_after_ms in
    let of_tuple ((), error, retry_after_ms) =
      {errcode= "M_LIMIT_EXCEEDED"; error; retry_after_ms} in
    let with_tuple =
      obj3
        (req "errcode" (constant "M_LIMIT_EXCEEDED"))
        (req "error" string) (opt "retry_after_ms" int) in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(
      pf ppf "{errcode: %s; error: %s; retry_after_ms: %a}" t.errcode t.error
        (Dump.option int) t.retry_after_ms)
end

module Auth_error = struct
  module Flow = struct
    type t = {stages: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.stages in
      let of_tuple v =
        let stages = v in
        {stages} in
      let with_tuple = obj1 (req "stages" (list string)) in
      conv to_tuple of_tuple with_tuple

    let pp ppf t = Fmt.(pf ppf "{stages: %a}" Dump.(list string) t.stages)
  end

  type t = {
    errcode: string option;
    error: string option;
    completed: string list option;
    flows: Flow.t list;
    params: (string * (string * string) list) list option;
    session: string option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.error, t.errcode, t.completed, t.flows, t.params, t.session in
    let of_tuple v =
      let error, errcode, completed, flows, params, session = v in
      {error; errcode; completed; flows; params; session} in
    let with_tuple =
      obj6 (opt "errcode" string) (opt "error" string)
        (opt "completed" (list string))
        (req "flows" (list Flow.encoding))
        (opt "params" (assoc (assoc string)))
        (opt "session" string) in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(
      pf ppf
        "{errcode: %a; error: %a; completed: %a; flows: %a; params: %a; \
         session: %a}"
        Dump.(option string)
        t.errcode
        Dump.(option string)
        t.error
        Dump.(option (list string))
        t.completed
        Dump.(list Flow.pp)
        t.flows
        Dump.(option (list (pair string (list (pair string string)))))
        t.params
        Dump.(option string)
        t.session)
end

type t =
  | Error of Error.t
  | Auth_error of Auth_error.t
  | Rate_limited of Rate_limited.t

let encoding =
  union
    [
      case Rate_limited.encoding
        (function Rate_limited t -> Some t | _ -> None)
        (fun t -> Rate_limited t);
      case Auth_error.encoding
        (function Auth_error t -> Some t | _ -> None)
        (fun t -> Auth_error t);
      case Error.encoding
        (function Error t -> Some t | _ -> None)
        (fun t -> Error t);
    ]

let pp ppf = function
  | Error t -> Fmt.pf ppf "Error of %a" Error.pp t
  | Auth_error t -> Fmt.pf ppf "Auth_error of %a" Auth_error.pp t
  | Rate_limited t -> Fmt.pf ppf "Rate_limited of %a" Rate_limited.pp t

exception Error_excpt of t
