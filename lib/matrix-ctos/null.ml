open Json_encoding

let string =
  union
    [ case string (function Some s -> Some s | _ -> None) (function s -> Some s)
    ; case null (function None -> Some () | _ -> assert false) (function () -> None) ]

let int =
  union
    [ case int (function Some s -> Some s | _ -> None) (function s -> Some s)
    ; case null (function None -> Some () | _ -> assert false) (function () -> None) ]
