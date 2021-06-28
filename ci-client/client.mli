type t = {
  host : string;
  port : int;
  device : string option;
  user : string;
  pwd : string;
}

val run: Current.Job.t -> string -> t -> string -> (unit, string) result Lwt.t
