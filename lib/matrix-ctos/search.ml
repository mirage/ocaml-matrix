open Json_encoding
open Matrix_common

module Query = struct
  type t = {next_batch: string option} [@@deriving accessor]

  let args t =
    match t.next_batch with
    | None -> []
    | Some next_batch -> ["next_batch", [next_batch]]
end

module Request = struct
  module Criteria = struct
    module Key = struct
      type t = Content_body | Content_name | Content_topic

      let encoding =
        string_enum
          [
            "content_body", Content_body; "content_name", Content_name
          ; "content_topic", Content_topic
          ]
    end

    module Filter = struct
      type t = {
          limit: int option
        ; not_senders: string list option
        ; not_types: string list option
        ; senders: string list option
        ; types: string list option
        ; lazy_load_members: bool option
        ; include_redundant_members: bool option
        ; not_rooms: string list option
        ; rooms: string list option
        ; contains_url: bool option
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          ( t.limit
          , t.not_senders
          , t.not_types
          , t.senders
          , t.types
          , t.lazy_load_members
          , t.include_redundant_members
          , t.not_rooms
          , t.rooms
          , t.contains_url ) in
        let of_tuple v =
          let ( limit
              , not_senders
              , not_types
              , senders
              , types
              , lazy_load_members
              , include_redundant_members
              , not_rooms
              , rooms
              , contains_url ) =
            v in
          {
            limit
          ; not_senders
          ; not_types
          ; senders
          ; types
          ; lazy_load_members
          ; include_redundant_members
          ; not_rooms
          ; rooms
          ; contains_url
          } in
        let with_tuple =
          obj10 (opt "limit" int)
            (opt "not_senders" (list string))
            (opt "not_types" (list string))
            (opt "senders" (list string))
            (opt "types" (list string))
            (opt "lazy_load_members" bool)
            (opt "include_redundant_members" bool)
            (opt "not_rooms" (list string))
            (opt "rooms" (list string))
            (opt "contains_url" bool) in
        conv to_tuple of_tuple with_tuple
    end

    module Order = struct
      type t = Recent | Rank

      let encoding = string_enum ["recent", Recent; "rank", Rank]
    end

    module Include_event_context = struct
      type t = {
          before_limit: int option
        ; after_limit: int option
        ; include_profile: bool option
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.before_limit, t.after_limit, t.include_profile in
        let of_tuple v =
          let before_limit, after_limit, include_profile = v in
          {before_limit; after_limit; include_profile} in
        let with_tuple =
          obj3 (opt "before_limit" int) (opt "after_limit" int)
            (opt "include_profile" bool) in
        conv to_tuple of_tuple with_tuple
    end

    module Groupings = struct
      module Group = struct
        type t = Room_id | Sender

        let encoding = string_enum ["room_id", Room_id; "sender", Sender]
      end

      type t = {group_by: Group.t list option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.group_by in
        let of_tuple v =
          let group_by = v in
          {group_by} in
        let with_tuple = obj1 (opt "group_by" (list Group.encoding)) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
        search_term: string
      ; keys: Key.t option
      ; filter: Filter.t option
      ; order_by: Order.t option
      ; event_context: Include_event_context.t option
      ; include_state: bool option
      ; groupings: Groupings.t option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.search_term
        , t.keys
        , t.filter
        , t.order_by
        , t.event_context
        , t.include_state
        , t.groupings ) in
      let of_tuple v =
        let ( search_term
            , keys
            , filter
            , order_by
            , event_context
            , include_state
            , groupings ) =
          v in
        {
          search_term
        ; keys
        ; filter
        ; order_by
        ; event_context
        ; include_state
        ; groupings
        } in
      let with_tuple =
        obj7 (req "search_term" string) (opt "keys" Key.encoding)
          (opt "filter" Filter.encoding)
          (opt "order_by" Order.encoding)
          (opt "event_context" Include_event_context.encoding)
          (opt "include_state" bool)
          (opt "groupings" Groupings.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {criterias: Criteria.t option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.criterias in
    let of_tuple v =
      let criterias = v in
      {criterias} in
    let with_tuple =
      obj1
        (req "search_categories" (obj1 (opt "room_events" Criteria.encoding)))
    in
    conv to_tuple of_tuple with_tuple
end

module Response = struct
  module Results = struct
    module Result = struct
      module Event_context = struct
        module User_profile = struct
          type t = {displayname: string option; avatar_url: string option}
          [@@deriving accessor]

          let encoding =
            let to_tuple t = t.displayname, t.avatar_url in
            let of_tuple v =
              let displayname, avatar_url = v in
              {displayname; avatar_url} in
            let with_tuple =
              obj2 (opt "displayname" string) (opt "avatar_url" string) in
            conv to_tuple of_tuple with_tuple
        end

        type t = {
            start: string option
          ; end_: string option
          ; profile_info: (string * User_profile.t) list option
          ; events_before: Events.Room_event.t list option
          ; events_after: Events.Room_event.t list option
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t =
            t.start, t.end_, t.profile_info, t.events_before, t.events_after
          in
          let of_tuple v =
            let start, end_, profile_info, events_before, events_after = v in
            {start; end_; profile_info; events_before; events_after} in
          let with_tuple =
            obj5 (opt "start" string) (opt "end" string)
              (opt "profile_info" (assoc User_profile.encoding))
              (opt "events_before" (list Events.Room_event.encoding))
              (opt "events_after" (list Events.Room_event.encoding)) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {
          rank: int option
        ; result: Events.Room_event.t option
        ; context: Event_context.t option
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.rank, t.result, t.context in
        let of_tuple v =
          let rank, result, context = v in
          {rank; result; context} in
        let with_tuple =
          obj3 (opt "rank" int)
            (opt "result" Events.Room_event.encoding)
            (opt "context" Event_context.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Group_value = struct
      type t = {
          next_batch: string option
        ; order: int option
        ; results: string list option
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.next_batch, t.order, t.results in
        let of_tuple v =
          let next_batch, order, results = v in
          {next_batch; order; results} in
        let with_tuple =
          obj3 (opt "next_batch" string) (opt "order" int)
            (opt "results" (list string)) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
        count: int option
      ; highlights: string list option
      ; results: Result.t list option
      ; state: (string * Events.State_event.t) list option
      ; groups: (string * (string * Group_value.t) list) list option
      ; next_batch: string option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.count, t.highlights, t.results, t.state, t.groups, t.next_batch in
      let of_tuple v =
        let count, highlights, results, state, groups, next_batch = v in
        {count; highlights; results; state; groups; next_batch} in
      let with_tuple =
        obj6 (opt "count" int)
          (opt "highlights" (list string))
          (opt "results" (list Result.encoding))
          (opt "state" (assoc Events.State_event.encoding))
          (opt "groups" (assoc (assoc Group_value.encoding)))
          (opt "next_batch" string) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {results: Results.t option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.results in
    let of_tuple v =
      let results = v in
      {results} in
    let with_tuple =
      obj1 (req "search_categories" (obj1 (opt "room_events" Results.encoding)))
    in
    conv to_tuple of_tuple with_tuple
end
