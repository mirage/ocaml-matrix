(* JSON structure description using dependently typed combinators. *)

(************************************************************************)
(*  ocplib-json-typed                                                   *)
(*                                                                      *)
(*    Copyright 2014 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocplib-json-typed is distributed in the hope that it will be useful,*)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

(*
 * Heavily based on ocp-json-typed, but with slight changes to correspond to my
 * needs. Should be entirely reworked
 *)

module Path =
struct
  type path =
    path_item list

  and path_item =
    [ `Field of string
    | `Index of int ]

  let print_path_item ppf = function
    | `Field s -> Fmt.pf ppf "/%s" s
    | `Index i -> Fmt.pf ppf "[%d]" i

  let pp =
    Fmt.list print_path_item

  exception Illegal_pointer_notation of string * int * string
  exception Unsupported_path_item of path_item * string
  exception Cannot_merge of path
end

exception Unexpected of string * string
exception No_case_matched of exn list
exception Bad_array_size of int * int
exception Missing_field of string
exception Unexpected_field of string
exception Bad_schema of exn
exception Cannot_destruct of (Path.path * exn)

module Repr = struct
  type value =
    [ `O of (string * value) list
    | `A of value list
    | `Bool of bool
    | `Float of float
    | `String of string
    | `Null ]
  let view v = v
  let repr v = v
end

let unexpected kind expected =
  let kind = match kind with
    | `O [] -> "empty object"
    | `A [] -> "empty array"
    | `O _ -> "object"
    | `A _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"
    | `Float _ -> "number"
    | `Bool _ -> "boolean" in
  Cannot_destruct ([], Unexpected (kind, expected))

type 't repr_agnostic_custom =
  { write : 't -> Repr.value ;
    read : Repr.value -> 't }

type _ encoding =
  | Null : unit encoding
  | Empty : unit encoding
  | Ignore : unit encoding
  | Option : 'a encoding -> 'a option encoding
  | Constant : string -> unit encoding
  | Int : int bounds -> int encoding
  | Bool : bool encoding
  | String : string encoding
  | Float : float bounds -> float encoding
  | Array : 'a encoding -> 'a array encoding
  | Obj : 'a field -> 'a encoding
  | Objs : 'a encoding * 'b encoding -> ('a * 'b) encoding
  | Custom : 't repr_agnostic_custom -> 't encoding
  | Conv : ('a -> 'b) * ('b -> 'a) * 'b encoding -> 'a encoding
  | Union : 't case list -> 't encoding
  | Cond : 'a encoding * ( 'a * 'b case) list -> ('a * 'b) encoding

and 'a bounds =
  { minimum : 'a option ;
    maximum : 'a option }

and _ field =
  | Req : { name: string ;
            encoding:  'a encoding ;
          } -> 'a field
  | Opt : { name: string ;
            encoding:  'a encoding ;
          } -> 'a option field
  | Dft : { name: string ;
            encoding:  'a encoding ;
            default: 'a ;
          } -> 'a field

and 't case =
  | Case : { encoding : 'a encoding ;
             proj : ('t -> 'a option) ;
             inj : ('a -> 't) } -> 't case

let construct enc v =
  let rec construct
    : type t. t encoding -> t -> Repr.value
    = function
      | Null -> (fun () -> Repr.repr `Null)
      | Empty -> (fun () -> Repr.repr (`O []))
      | Ignore -> (fun () -> Repr.repr (`O []))
      | Option t ->
        (function
          | None -> Repr.repr `Null
          | Some v -> construct t v)
      | Constant str -> (fun () -> Repr.repr (`String str))
      | Int { minimum ; maximum } ->
        let err = "Json_encoding.construct: int out of range" in
        (fun int ->
           if (Option.is_some minimum && int < Option.get minimum )
           || (Option.is_some maximum && int > Option.get maximum )
           then invalid_arg err ;
           Repr.repr (`Float (float_of_int int)))
      | Bool -> (fun (b : t) -> Repr.repr (`Bool b))
      | String -> (fun s -> Repr.repr (`String s))
      | Float { minimum ; maximum } ->
        let err = "Json_encoding.construct: float out of range" in
        (fun float ->
           if (Option.is_some minimum && float < Option.get minimum )
           || (Option.is_some maximum && float > Option.get maximum )
           then invalid_arg err ;
           Repr.repr (`Float float))
      | Custom { write; _ } -> (fun (j : t) -> write j)
      | Conv (ffrom, _, t) -> (fun v -> construct t (ffrom v))
      | Array t ->
        let w v = construct t v in
        (fun arr -> Repr.repr (`A (Array.to_list (Array.map w arr))))
      | Obj (Req { name = n ; encoding = t }) ->
        let w v = construct t v in
        (fun v -> Repr.repr (`O [ n, w v ]))
      | Obj (Dft { name = n ; encoding = t ; default = d }) ->
        let w v = construct t v in
        (fun v -> Repr.repr (`O (if v <> d then [ n, w v ] else [])))
      | Obj (Opt { name = n ; encoding = t }) ->
        let w v = construct t v in
        (function None -> Repr.repr (`O []) | Some v -> Repr.repr (`O [ n, w v ]))
      | Objs (o1, o2) ->
        let w1 v = construct o1 v in
        let w2 v = construct o2 v in
        (function (v1, v2) ->
         match Repr.view (w1 v1), Repr.view (w2 v2) with
         | `O l1, `O l2 -> Repr.repr (`O (l1 @ l2))
         | `Null, `Null
         | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_objs")
      | Union cases ->
        (fun v ->
           let rec do_cases = function
             | [] -> invalid_arg "Json_encoding.construct: consequence of bad union"
             | Case { encoding ; proj ; _ } :: rest ->
               match proj v with
               | Some v -> construct encoding v
               | None -> do_cases rest in
           do_cases cases)
      | Cond (t, cases) ->
        let w k = construct t k in
        let wv k v =
          let case = List.assoc k cases in
          match case with
          | Case { encoding ; proj ; _ } ->
            (match proj v with
            | Some v -> construct encoding v
            | None -> invalid_arg "RIP")
        in
        (function (k, v) ->
          match Repr.view (w k), Repr.view (wv k v) with
          | `O l1, `O l2 -> Repr.repr (`O (l1 @ l2))
          | _ -> invalid_arg "Json_encoding.construct: consequence of bad merge_objs") in
  construct enc v

let rec destruct
  : type t. t encoding -> (Repr.value -> t)
  = function
    | Null -> (fun v -> match Repr.view v with `Null -> () | k -> raise (unexpected k "null"))
    | Empty -> (fun v -> match Repr.view v with
        | `O [] -> ()
        | `O [ f, _] -> raise (Cannot_destruct ([], Unexpected_field f))
        | k -> raise @@ unexpected k "an empty object")
    | Ignore -> (fun v -> match Repr.view v with _ -> ())
    | Option t -> (fun v -> match Repr.view v with
        | `Null -> None
        | _ -> Some (destruct t v))
    | Constant str ->
      (fun v ->
         match Repr.view v with
         | `String s when s = str -> ()
         | x -> raise @@ unexpected x str)
    | Int { minimum ; maximum } ->
      (fun v ->
         match Repr.view v with
         | `Float v ->
           let rest, v = modf v in
           if rest <> 0. then begin
             let exn = Failure ("int cannot have a fractional part") in
             raise (Cannot_destruct ([], exn))
           end ;
           if (Option.is_some minimum && v < (Option.get minimum |> float_of_int))
           || (Option.is_some maximum && v > (Option.get maximum |> float_of_int))
           then begin
             let exn = Failure ("int out of range") in
             raise (Cannot_destruct ([], exn))
           end ;
           int_of_float v
         | k -> raise (unexpected k "number"))
    | Bool -> (fun v -> match Repr.view v with `Bool b -> (b : t) | k -> raise (unexpected k "boolean"))
    | String -> (fun v -> match Repr.view v with `String s -> s | k -> raise (unexpected k "string"))
    | Float { minimum ; maximum } ->
      (fun v ->
         match Repr.view v with
         | `Float f ->
           if (Option.is_some minimum && f < Option.get minimum)
           || (Option.is_some maximum && f > Option.get maximum)
           then
             let exn = Failure ("float out of range") in
             raise (Cannot_destruct ([], exn))
           else f
         | k -> raise (unexpected k "float"))
    | Custom { read; _ } -> read
    | Conv (_, fto, t) -> (fun v -> fto (destruct t v))
    | Array t ->
      (fun v -> match Repr.view v with
         | `O [] ->
           [||]
         | `A cells ->
           Array.mapi
             (fun i cell ->
                try destruct t cell with Cannot_destruct (path, err) ->
                  raise (Cannot_destruct (`Index i :: path, err)))
             (Array.of_list cells)
         | k -> raise @@ unexpected k "array")
    | Obj _  as t ->
      let d = destruct_obj t in
      (fun v -> match Repr.view v with
         | `O fields ->
           let r, rest, _ign = d fields in
           begin match rest with
             (* | (field, _) :: _ when not ign -> raise @@ Unexpected_field field *)
             | _ -> r
           end
         | k -> raise @@ unexpected k "object")
    | Objs _ as t ->
      let d = destruct_obj t in
      (fun v -> match Repr.view v with
         | `O fields ->
           let r, rest, _ign = d fields in
           begin match rest with
             (* | (field, _) :: _ when not ign -> raise @@ Unexpected_field field *)
             | _ -> r
           end
         | k -> raise @@ unexpected k "object")
    | Union cases ->
      (fun v ->
         let rec do_cases errs = function
           | [] -> raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
           | Case { encoding ; inj ; _ } :: rest ->
             try inj (destruct encoding v) with
               err -> do_cases (err :: errs) rest in
         do_cases [] cases)
    | Cond (t, cases) ->
      let d = destruct_obj t in
      (fun v -> match Repr.view v with
         | `O fields ->
           let r, rest, _ign = d fields in
           (match List.assoc_opt r cases with
           | Some (Case { encoding ; inj ; _ }) ->
              r, inj (destruct encoding (`O rest))
           | None -> raise (Cannot_destruct ([], No_case_matched [Invalid_argument "to do: Cond destruct error"])))
         | k -> raise @@ unexpected k "object")
and destruct_obj
  : type t. t encoding -> (string * Repr.value) list -> t * (string * Repr.value) list * bool
  = fun t ->
    let rec assoc acc n = function
      | [] -> raise Not_found
      | (f, v) :: rest when n = f -> v, acc @ rest
      | oth :: rest -> assoc (oth :: acc) n rest in
    match t with
    | Empty -> (fun fields -> (), fields, false)
    | Ignore -> (fun fields -> (), fields, true)
    | Obj (Req { name = n ; encoding = t }) ->
      (fun fields ->
         try
           let v, rest = assoc [] n fields in
           destruct t v, rest, false
         with
         | Not_found ->
           raise (Cannot_destruct ([], Missing_field n))
         | Cannot_destruct (path, err) ->
           raise (Cannot_destruct (`Field n :: path, err)))
    | Obj (Opt { name = n ; encoding = t }) ->
      (fun fields ->
         try
           let v, rest = assoc [] n fields in
           Some (destruct t v), rest, false
         with
         | Not_found -> None, fields, false
         | Cannot_destruct (path, err) ->
           raise (Cannot_destruct (`Field n :: path, err)))
    | Obj (Dft { name = n ; encoding = t ; default = d }) ->
      (fun fields ->
         try
           let v, rest = assoc [] n fields in
           destruct t v, rest, false
         with
         | Not_found -> d, fields, false
         | Cannot_destruct (path, err) ->
           raise (Cannot_destruct (`Field n :: path, err)))
    | Objs (o1, o2) ->
      let d1 = destruct_obj o1 in
      let d2 = destruct_obj o2 in
      (fun fields ->
         let r1, rest, ign1 = d1 fields in
         let r2, rest, ign2 = d2 rest in
         (r1, r2), rest, ign1 || ign2)
    | Conv (_, fto, t) ->
      let d = destruct_obj t in
      (fun fields ->
         let r, rest, ign = d fields in
         fto r, rest, ign)
    | Union cases ->
      (fun fields ->
         let rec do_cases errs = function
           | [] -> raise (Cannot_destruct ([], No_case_matched (List.rev errs)))
           | Case { encoding ; inj ; _ } :: rest ->
             try
               let r, rest, ign = destruct_obj encoding fields in
               inj r, rest, ign
             with err -> do_cases (err :: errs) rest in
         do_cases [] cases)
    | Cond (t, cases) ->
      let d = destruct_obj t in
      (fun fields ->
        let r1, rest, ign1 = d fields in
        (match List.assoc_opt r1 cases with
          | Some (Case { encoding ; inj ; _ }) ->
            let r, rest, ign = destruct_obj encoding rest in
            let r2, rest, ign2 = inj r, rest, ign in
            (r1, r2), rest, ign1 || ign2
          | None -> raise (Cannot_destruct ([], No_case_matched [Invalid_argument "to do: Cond destruct error"]))))
    | _ -> invalid_arg "Json_encoding.destruct: consequence of bad merge_objs"

let req n t =
  Req { name = n ; encoding = t }
let opt n t =
  Opt { name = n ; encoding = t }
let dft n t d =
  Dft { name = n ; encoding = t ; default = d }

let null = Null

let int =
  Int { minimum = None ;
        maximum = None }

let ranged_int minimum maximum =
  Int { minimum ;
        maximum }

let float =
  Float { minimum = None ;
          maximum = None }

let ranged_float minimum maximum =
  Float { minimum ;
          maximum }

let string = String
let conv ffrom fto t =
  Conv (ffrom, fto, t)
let bool = Bool
let array t = Array t
let obj1 f1 = Obj f1
let obj2 f1 f2 = Objs (Obj f1, Obj f2)
let obj3 f1 f2 f3 =
  conv
    (fun (a, b, c) -> (a, (b, c)))
    (fun (a, (b, c)) -> (a, b, c))
    (Objs (Obj f1, Objs (Obj f2, Obj f3)))
let obj4 f1 f2 f3 f4 =
  conv
    (fun (a, b, c, d) -> (a, (b, (c, d))))
    (fun (a, (b, (c, d))) -> (a, b, c, d))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Obj f4))))
let obj5 f1 f2 f3 f4 f5 =
  conv
    (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
    (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Obj f5)))))
let obj6 f1 f2 f3 f4 f5 f6 =
  conv
    (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
    (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
    (Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, Obj f6))))))
let obj7 f1 f2 f3 f4 f5 f6 f7 =
  conv
    (fun (a, b, c, d, e, f, g) -> (a, (b, (c, (d, (e, (f, g)))))))
    (fun (a, (b, (c, (d, (e, (f, g)))))) -> (a, b, c, d, e, f, g))
    (let rest = Objs (Obj f6, Obj f7) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj8 f1 f2 f3 f4 f5 f6 f7 f8 =
  conv
    (fun (a, b, c, d, e, f, g, h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, h))))))) -> (a, b, c, d, e, f, g, h))
    (let rest = Objs (Obj f6, Objs (Obj f7, Obj f8)) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj9 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  conv
    (fun (a, b, c, d, e, f, g, h, i) -> (a, (b, (c, (d, (e, (f, (g, (h, i)))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) -> (a, b, c, d, e, f, g, h, i))
    (let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Obj f9))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))
let obj10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
  conv
    (fun (a, b, c, d, e, f, g, h, i, j) -> (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))))
    (fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) -> (a, b, c, d, e, f, g, h, i, j))
    (let rest = Objs (Obj f6, Objs (Obj f7, Objs (Obj f8, Objs (Obj f9, Obj f10)))) in
     Objs (Obj f1, Objs (Obj f2, Objs (Obj f3, Objs (Obj f4, Objs (Obj f5, rest))))))

let constant s = Constant s

let string_enum cases =
  let len = List.length cases in
  let mcases = Hashtbl.create len
  and rcases = Hashtbl.create len in
  let cases_str = String.concat " " (List.map (fun x -> "'" ^ fst x ^ "'") cases) in
  List.iter
    (fun (s, c) ->
       if Hashtbl.mem mcases s then
         invalid_arg "Json_encoding.string_enum: duplicate case" ;
       Hashtbl.add mcases s c ;
       Hashtbl.add rcases c s)
    cases ;
  conv
    (fun v -> try Hashtbl.find rcases v with Not_found ->
        invalid_arg (Fmt.str "Json_encoding.construct: consequence of non exhaustive Json_encoding.string_enum. Strings are: %s" cases_str))
    (fun s ->
       (try Hashtbl.find mcases s with Not_found ->
          let rec orpat ppf = function
            | [] -> assert false
            | [ last, _ ] -> Fmt.pf ppf "%S" last
            | [ prev, _ ; last, _ ] -> Fmt.pf ppf "%S or %S" prev last
            | (prev, _) :: rem -> Fmt.pf ppf "%S , %a" prev orpat rem in
          let unexpected = Fmt.str "string value %S" s in
          let expected = Fmt.str "%a" orpat cases in
          raise (Cannot_destruct ([], Unexpected (unexpected, expected)))))
    string

let assoc t =
  let write l =
    `O (List.map (fun (n, v) -> n, construct t v) l)
  in
  let read = function
    | `O l ->
      let destruct n t v = try
          destruct t v
        with Cannot_destruct (p, exn) -> raise (Cannot_destruct (`Field n :: p, exn)) in
      List.map (fun (n, v) -> n, destruct n t v) l
    | #Repr.value as k -> raise (unexpected k "asssociative object")
  in
  Custom { read; write }

let option : type t. t encoding -> t option encoding = fun t ->
  Option t

let any =
  let id x = x in
  Custom { read = id ; write = id }

let list t =
  Conv (Array.of_list, Array.to_list, Array t)

let merge_objs o1 o2 =
  (* FIXME: check fields unicity *)
  let rec is_obj : type t. t encoding -> bool = function
    | Obj _ -> true
    | Objs _ -> true
    | Conv (_, _, t) -> is_obj t
    | Empty -> true
    | Ignore -> true
    | Union cases -> List.for_all (fun (Case { encoding = o; _ }) -> is_obj o) cases
    | Cond (k, cases) -> is_obj k && List.for_all (fun (_, Case { encoding = o; _ }) -> is_obj o) cases
    | _ -> false in
  if is_obj o1 && is_obj o2 then
    Objs (o1, o2)
  else
    invalid_arg "Json_encoding.merge_objs"

let empty =
  Empty

let unit =
  Ignore

let case encoding proj inj  =
  Case { encoding ; proj ; inj }

let union = function
  | [] -> invalid_arg "Json_encoding.union"
  | cases ->
    (* FIXME: check mutual exclusion *)
    Union cases

let cond k = function
  | [] -> invalid_arg "Json_encoding.union2"
  | cases ->
    (* FIXME: check mutual exclusion *)
    Cond (k, cases)

let rec print_error ppf = function
  | Cannot_destruct ([], exn) ->
    print_error ppf exn
  | Cannot_destruct (path, Unexpected (unex, ex)) ->
    Fmt.pf ppf
      "At %a, unexpected %s instead of %s"
      Path.pp path
      unex ex
  | Cannot_destruct (path, No_case_matched errs) ->
    Fmt.pf ppf
      "@[<v 2>At %a, no case matched:@,%a@]"
      Path.pp path
      (Format.pp_print_list print_error) errs
  | Cannot_destruct (path, Bad_array_size (unex, ex)) ->
    Fmt.pf ppf
      "At %a, unexpected array of size %d instead of %d"
      Path.pp path
      unex ex
  | Cannot_destruct (path, Missing_field n) ->
    Fmt.pf ppf
      "At %a, missing object field %s"
      Path.pp path
      n
  | Cannot_destruct (path, Unexpected_field n) ->
    Fmt.pf ppf
      "At %a, unexpected object field %s"
      Path.pp path
      n
  | Cannot_destruct (path, Bad_schema exn) ->
    Fmt.pf ppf
      "@[<v 2>At %a, bad custom schema:@,%a@]"
      Path.pp path
      print_error exn
  | Unexpected (unex, ex) ->
    Fmt.pf ppf
      "Unexpected %s instead of %s" unex ex
  | No_case_matched errs ->
    Fmt.pf ppf
      "@[<v 2>No case matched:@,%a@]"
      (Format.pp_print_list print_error) errs
  | Bad_array_size (unex, ex) ->
    Fmt.pf ppf
      "Unexpected array of size %d instead of %d" unex ex
  | Missing_field n ->
    Fmt.pf ppf
      "Missing object field %s" n
  | Unexpected_field n ->
    Fmt.pf ppf
      "Unexpected object field %s" n
  | Bad_schema exn ->
    Fmt.pf ppf
      "@[<v 2>bad custom schema:@,%a@]"
      print_error exn
  | Cannot_destruct (path, exn) ->
    Fmt.pf ppf
      "@[<v 2>At %a:@,%a@]"
      Path.pp path
      print_error exn
  | exn ->
    Fmt.pf ppf "%a" Fmt.exn exn

(* simply added to log directly the errors, should be done in the code *)
let destruct e v =
  try
    destruct e v
  with
    | e ->
      Logs.err (fun m -> m "Json exception: %a" print_error e);
      raise e
