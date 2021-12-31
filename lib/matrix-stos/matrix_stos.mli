open Json_encoding
open Matrix_common

module Backfill : sig
  module Query : sig
    type%accessor t = {v: string list; limit: int}

    val args : t -> (string * string list) list
  end

  module Response : sig
    type%accessor t = {origin: string; origin_server_ts: int; pdus: Pdu.t list}

    val encoding : t encoding
  end
end

module Get_missing_events : sig
  module Request : sig
    type%accessor t = {
      limit: int option;
      min_depth: int option;
      earliest_events: string list;
      lastest_events: string list;
    }

    val encoding : t encoding
  end

  module Response : sig
    type%accessor t = {events: Pdu.t list}

    val encoding : t encoding
  end
end

module Event_auth : sig
  module Query = Empty.Query

  module Response : sig
    type%accessor t = {auth_chain: Events.Pdu.t list}

    val encoding : t encoding
  end
end

module Key : sig
  module Server_key : sig
    module Verify_key : sig
      type%accessor t = {key: string}

      val encoding : t encoding
    end

    module Old_verify_key : sig
      type%accessor t = {key: string; expired_ts: int}

      val encoding : t encoding
    end

    type%accessor t = {
      server_name: string;
      verify_keys: (string * Verify_key.t) list;
      old_verify_keys: (string * Old_verify_key.t) list option;
      signatures: (string * (string * string) list) list option;
      valid_until_ts: int option;
    }

    val encoding : t encoding
  end

  module Direct_query : sig
    module Query = Empty.Query module Response = Server_key
  end

  module Indirect_query : sig
    module Query : sig
      type%accessor t = {minimum_valid_until_ts: int option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {server_keys: Server_key.t list}

      val encoding : t encoding
    end
  end

  module Indirect_batch_query : sig
    module Query = Empty.Query

    module Request : sig
      module Query_criteria : sig
        type%accessor t = {minimum_valid_until_ts: int option}

        val encoding : t encoding
      end

      type%accessor t = {
        server_keys: (string * (string * Query_criteria.t) list) list;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {server_keys: Server_key.t list}

      val encoding : t encoding
    end
  end
end

module Public_rooms : sig
  module Get_public_rooms : sig
    module Query : sig
      type%accessor t = {
        limit: int option;
        since: string option;
        include_all_networks: bool option;
        third_party_instance_id: string option;
      }

      val args : t -> (string * string list) list
    end

    module Response : sig
      module Public_rooms_chunk : sig
        type%accessor t = {
          aliases: string list option;
          canonical_alias: string option;
          name: string option;
          num_joined_members: int;
          room_id: string;
          topic: string option;
          world_readable: bool;
          guest_can_join: bool;
          avatar_url: string option;
          federate: bool option;
        }

        val encoding : t encoding
      end

      type%accessor t = {
        chunk: Public_rooms_chunk.t list;
        next_batch: string option;
        prev_batch: string option;
        total_room_count_estimate: int option;
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
        limit: int option;
        since: string option;
        filter: Filter.t option;
        include_all_networks: bool option;
        third_party_instance_id: string option;
      }

      val encoding : t encoding
    end

    module Response = Get_public_rooms.Response
  end
end

module Query : sig
  module Directory : sig
    module Response : sig
      type%accessor t = {room_id: string; servers: string list}

      val encoding : t encoding
    end
  end

  module Profile : sig
    module Response : sig
      type%accessor t = {avatar_url: string option; displayname: string option}

      val encoding : t encoding
    end
  end
end

module Joining_rooms : sig
  module Make_join : sig
    module Query : sig
      type%accessor t = {ver: string list option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {
        room_version: string option;
        event_template: Pdu.t option;
      }

      val encoding : t encoding
    end
  end

  module Send_join : sig
    module V1 : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          sender: string;
          origin: string;
          origin_server_ts: int;
          event_type: string;
          state_key: string;
          content: Events.Event_content.Member.t;
        }

        val encoding : t encoding
      end

      module Response : sig
        module Room_state : sig
          type%accessor t = {
            origin: string;
            auth_chain: Events.Pdu.t list;
            state: Events.Pdu.t list;
          }

          val encoding : t encoding
        end

        type%accessor t = {room_state: Room_state.t option}

        val encoding : t encoding
      end
    end

    module V2 : sig
      module Query : Empty.QUERY
      module Request = Matrix_common.Events.Pdu

      module Response : sig
        type%accessor t = {
          origin: string;
          auth_chain: Events.Pdu.t list;
          state: Events.Pdu.t list;
        }

        val encoding : t encoding
      end
    end
  end
end

module Federation_request : sig
  type%accessor t = {
    meth: string;
    uri: string;
    origin: string;
    destination: string option;
    content: Ezjsonm.value option;
  }

  val encoding : t encoding
end

module Send : sig
  module Request : sig
    type%accessor t = {
      origin: string;
      origin_server_ts: int;
      pdus: Pdu.t list;
      edus: Edu.t list option;
    }

    val encoding : t encoding
  end

  module Response : sig
    module Pdu_processing_result : sig
      type%accessor t = {error: string option}

      val encoding : t encoding
    end

    type%accessor t = {pdus: (string * Pdu_processing_result.t) list}

    val encoding : t encoding
  end
end

module Signatures : sig
  val encoding :
    (string * (string * Mirage_crypto_ec.Ed25519.priv) list) list ->
    'a encoding ->
    'a encoding
end

module Retrieve : sig
  module State : sig
    module Query : sig
      type%accessor t = {event_id: string}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {
        auth_chain: Events.State_event.t list;
        pdus: Events.State_event.t list;
      }

      val encoding : t encoding
    end
  end

  module State_ids : sig
    module Query : sig
      type%accessor t = {event_id: string}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {auth_chain_ids: string list; pdus_ids: string list}

      val encoding : t encoding
    end
  end

  module Event : sig
    module Query = Matrix_common.Empty.Query

    module Response : sig
      type%accessor t = {
        origin: string;
        origin_server_ts: int;
        pdus: Pdu.t list;
      }

      val encoding : t encoding
    end
  end
end

module User : sig
  module Devices : sig
    module Response : sig
      module User_device : sig
        module Device_keys : sig
          type%accessor t = {
            algorithms: string list;
            device_id: string;
            keys: (string * string) list;
            signatures: (string * (string * string) list) list;
            user_id: string;
          }

          val encoding : t encoding
        end

        type%accessor t = {
          device_display_name: string option;
          device_id: string;
          keys: Device_keys.t;
        }

        val encoding : t encoding
      end

      module Cross_signing_key : sig
        type%accessor t = {
          keys: (string * string) list;
          signatures: (string * (string * string) list) list;
          usage: string list;
          user_id: string;
        }

        val encoding : t encoding
      end

      type%accessor t = {
        devices: User_device.t list;
        master_key: Cross_signing_key.t option;
        self_signing_key: Cross_signing_key.t option;
        stream_id: int;
        user_id: string;
      }

      val encoding : t encoding
    end
  end
end

module Version : sig
  module Query = Matrix_common.Empty.Query

  module Response : sig
    type%accessor t = {name: string option; version: string option}

    val encoding : t encoding
  end
end
