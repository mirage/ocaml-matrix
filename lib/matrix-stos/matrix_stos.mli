open Json_encoding
open Matrix_common

module Public_rooms : sig
  module Get_public_rooms : sig
    module Query : sig
      type%accessor t = {
          limit: int option
        ; since: string option
        ; include_all_networks: bool option
        ; third_party_instance_id: string option
      }

      val args : t -> (string * string list) list
    end

    module Response : sig
      module Public_rooms_chunk : sig
        type%accessor t = {
            aliases: string list option
          ; canonical_alias: string option
          ; name: string option
          ; num_joined_members: int
          ; room_id: string
          ; topic: string option
          ; world_readable: bool
          ; guest_can_join: bool
          ; avatar_url: string option
          ; federate: bool option
        }

        val encoding : t encoding
      end

      type%accessor t = {
          chunk: Public_rooms_chunk.t list
        ; next_batch: string option
        ; prev_batch: string option
        ; total_room_count_estimate: int option
      }

      val encoding : t encoding
    end
  end

  module Filter_public_rooms : sig
    module Query : Empty.QUERY

    module Request : sig
      module Filter : sig
        type%accessor t = {generic_search_term: string option}

        val encoding : t encoding
      end

      type%accessor t = {
          limit: int option
        ; since: string option
        ; filter: Filter.t option
        ; include_all_networks: bool option
        ; third_party_instance_id: string option
      }

      val encoding : t encoding
    end

    module Response = Get_public_rooms.Response
  end
end

module Joining_rooms : sig
  module Make_join : sig
    module Query : sig
      type%accessor t = {ver: string list option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      module Event_template : sig
        type%accessor t = {
            sender: string
          ; origin: string
          ; origin_server_ts: int
          ; event_type: string
          ; state_key: string
          ; room_id: string option
        }

        val encoding : t encoding
      end

      type%accessor t = {
          room_version: string option
        ; event_template: Event_template.t option
      }

      val encoding : t encoding
    end
  end

  module Send_join : sig
    module V1 : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
            sender: string
          ; origin: string
          ; origin_server_ts: int
          ; event_type: string
          ; state_key: string
          ; content: Events.Event_content.Member.t
        }

        val encoding : t encoding
      end

      module Response : sig
        module Room_state : sig
          type%accessor t = {
              origin: string
            ; auth_chain: Events.Pdu.t list
            ; state: Events.Pdu.t list
          }

          val encoding : t encoding
        end

        type%accessor t = {room_state: Room_state.t option}

        val encoding : t encoding
      end
    end

    module V2 : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
            sender: string
          ; origin: string
          ; origin_server_ts: int
          ; event_type: string
          ; state_key: string
          ; content: Events.Event_content.Member.t
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {
            origin: string
          ; auth_chain: Events.Pdu.t list
          ; state: Events.Pdu.t list
        }

        val encoding : t encoding
      end
    end
  end
end
