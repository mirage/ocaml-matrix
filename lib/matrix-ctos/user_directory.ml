open Json_encoding
open Matrix_common

module Search =
struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/user_directory/search"

  module Request =
  struct
    type t =
      { search_term: string
      ; limited: int option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.search_term, t.limited
      in
      let of_tuple v =
        let search_term, limited = v in
        {search_term; limited}
      in
      let with_tuple =
        obj2
          (req "search_term"
            string)
          (opt "limited"
            int)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response =
  struct
    module User =
    struct
      type t =
        { user_id: string
        ; display_name: string option
        ; avatar_url: string option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.user_id, t.display_name, t.avatar_url
        in
        let of_tuple v =
          let user_id, display_name, avatar_url = v in
          { user_id; display_name; avatar_url }
        in
        let with_tuple =
          obj3
            (req "user_id" string)
            (opt "display_name" string)
            (opt "avatar_url" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { results: User.t list
      ; limited: bool
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.results, t.limited
      in
      let of_tuple v =
        let results, limited = v in
        { results; limited }
      in
      let with_tuple =
        obj2
          (req "results" (list User.encoding))
          (req "limited" bool)
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
