open Json_encoding
open Matrix_common

let raw = any

module Upload = struct
  module Query = struct
    type t = {filename: string option} [@@deriving accessor]

    let args t =
      match t.filename with
      | None -> []
      | Some filename -> ["filename", [filename]]

    module Header = struct
      type t = {content_type: string; content_length: int} [@@deriving accessor]

      let header t =
        [
          "Content-Type", t.content_type;
          "Content-Length", Int.to_string t.content_length;
        ]
    end
  end

  module Request = struct
    type t = {file: string} [@@deriving accessor]

    let to_string t = t.file
  end

  module Response = struct
    type t = {content_uri: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.content_uri in
      let of_tuple v =
        let content_uri = v in
        {content_uri} in
      let with_tuple = obj1 (req "content_uri" string) in
      conv to_tuple of_tuple with_tuple
  end
end

module Download = struct
  module Query = struct
    type t = {allow_remote: bool option} [@@deriving accessor]

    let args t =
      match t.allow_remote with
      | None -> []
      | Some allow_remote -> ["allow_remote", [Bool.to_string allow_remote]]

    (* handle the header from the response *)
  end

  module Response = struct
    type t = {file: string} [@@deriving accessor]

    let of_string file = {file}
  end
end

module Download_filename = struct include Download end

module Thumbnail = struct
  module Query = struct
    module Rezising = struct
      type t = Crop | Scale

      let to_string = function Crop -> "crop" | Scale -> "scale"
    end

    type t = {
      width: int;
      height: int;
      rezising_method: Rezising.t option;
      allow_remote: bool option;
    }
    [@@deriving accessor]

    let args t =
      let l =
        ["width", [Int.to_string t.width]; "height", [Int.to_string t.height]]
      in
      let l =
        match t.rezising_method with
        | None -> l
        | Some rezising_method ->
          ("method", [Rezising.to_string rezising_method]) :: l in
      match t.allow_remote with
      | None -> l
      | Some allow_remote ->
        ("allow_remote", [Bool.to_string allow_remote]) :: l

    (* handle the header from the response *)
  end

  module Response = struct
    type t = {thumbnail: string} [@@deriving accessor]

    let of_string thumbnail = {thumbnail}
  end
end

module Preview = struct
  module Query = struct
    type t = {url: string; ts: int option} [@@deriving accessor]

    let args t =
      let l = ["url", [t.url]] in
      match t.ts with None -> l | Some ts -> ("ts", [Int.to_string ts]) :: l
  end

  module Response = struct
    type t = {infos: (string * Ezjsonm.value) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.infos in
      let of_tuple v =
        let infos = v in
        {infos} in
      let with_tuple = assoc any in
      conv to_tuple of_tuple with_tuple
  end
end

module Config = struct
  module Query = Empty.Query

  module Response = struct
    type t = {upload_size: int option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.upload_size in
      let of_tuple v =
        let upload_size = v in
        {upload_size} in
      let with_tuple = obj1 (opt "m.upload.size" int) in
      conv to_tuple of_tuple with_tuple
  end
end
