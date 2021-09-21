open Json_encoding
open Matrix_common

module Account_data : sig
  module Put : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {data: Ezjsonm.value}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {data: Ezjsonm.value}

      val encoding : t encoding
    end
  end

  module Put_by_room : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {data: (string * string) list}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get_by_room : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {data: (string * string) list}

      val encoding : t encoding
    end
  end
end

module Identifier : sig
  module User : sig
    type%accessor t = {user: string}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Thirdparty : sig
    type%accessor t = {medium: string; address: string}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Phone : sig
    type%accessor t = {country: string; phone: string}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  type t = User of User.t | Thirdparty of Thirdparty.t | Phone of Phone.t

  val encoding : t encoding
  val pp : t Fmt.t
end

module Authentication : sig
  module Dummy : sig
    type t = unit

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Password : sig
    module V1 : sig
      type%accessor t = {user: string; password: string}

      val encoding : t encoding
      val pp : t Fmt.t
    end

    module V2 : sig
      type%accessor t = {identifier: Identifier.t; password: string}

      val encoding : t encoding
      val pp : t Fmt.t
    end

    type t = V1 of V1.t | V2 of V2.t

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Token : sig
    type%accessor t = {token: string; txn_id: string}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Empty : Empty.JSON

  type t =
    | Dummy of Dummy.t
    | Password of Password.t
    | Token of Token.t
    | Empty of Empty.t

  val encoding : t encoding
  val pp : t Fmt.t
end

module Account : sig
  module Unbind_result : sig
    type t = Success | No_support

    val encoding : t encoding
  end

  module Password : sig
    module Post : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          auth: Authentication.Password.V1.t;
          new_password: string;
        }

        val encoding : t encoding
      end

      module Response : Empty.JSON
    end

    module Email_request_token : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          client_secret: string;
          email: string;
          send_attempt: int;
          next_link: string option;
          id_server: string;
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {sid: string; submit_url: string option}

        val encoding : t encoding
      end
    end

    module Msisdn_request_token : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          client_secret: string;
          country: string;
          phone_number: string;
          send_attempt: int;
          next_link: string option;
          id_server: string;
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {sid: string; submit_url: string option}

        val encoding : t encoding
      end
    end
  end

  module Deactivate : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        auth: Authentication.Password.V1.t;
        id_server: string option;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {id_server_unbind_result: Unbind_result.t}

      val encoding : t encoding
    end
  end

  module Third_party_id : sig
    module Medium : sig
      type t = Email | Msisdn

      val encoding : t encoding
    end

    module Get : sig
      module Query : Empty.QUERY

      module Response : sig
        module Third_party_identifier : sig
          type%accessor t = {
            medium: Medium.t;
            address: string;
            validated_at: int;
            added_at: int;
          }

          val encoding : t encoding
        end

        type%accessor t = {threepids: Third_party_identifier.t list}

        val encoding : t encoding
      end
    end

    module Post : sig
      module Query : Empty.QUERY

      module Request : sig
        module Three_pid_creds : sig
          type%accessor t = {
            client_secret: string;
            id_server: string;
            sid: string;
          }

          val encoding : t encoding
        end

        type%accessor t = {
          three_pid_creds: Three_pid_creds.t;
          bind: bool option;
        }

        val encoding : t encoding
      end

      module Response : Empty.JSON
    end

    module Delete : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          id_server: string option;
          medium: Medium.t;
          address: string;
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {id_server_unbind_result: Unbind_result.t}

        val encoding : t encoding
      end
    end

    module Email_request_token : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          client_secret: string;
          email: string;
          send_attempt: int;
          next_link: string option;
          id_server: string;
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {sid: string; submit_url: string option}

        val encoding : t encoding
      end
    end

    module Msisdn_request_token : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {
          client_secret: string;
          country: string;
          phone_number: string;
          send_attempt: int;
          next_link: string option;
          id_server: string;
        }

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {sid: string; submit_url: string option}

        val encoding : t encoding
      end
    end
  end

  module Whoami : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON

    module Response : sig
      type%accessor t = {user_id: string}

      val encoding : t encoding
    end
  end
end

module Audio_info : sig
  type%accessor t = {
    mimetype: string option;
    duration: int option;
    size: int option;
  }

  val encoding : t encoding
end

module Banning : sig
  module Ban : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {reason: string option; user_id: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Unban : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {user_id: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end
end

module Capabilities : sig
  module Capability : sig
    module Change_password : sig
      type%accessor t = {enabled: bool}

      val encoding : t encoding
    end

    module Room_versions : sig
      module Stability : sig
        type t = Stable | Unstable

        val encoding : t encoding
      end

      type%accessor t = {
        default: string;
        available: (string * Stability.t) list;
      }

      val encoding : t encoding
    end

    module Custom : sig
      type%accessor t = {name: string; content: Ezjsonm.value}

      val encoding : t encoding
    end

    type t =
      | Change_password of Change_password.t
      | Room_versions of Room_versions.t
      | Custom of Custom.t

    val encoding : t encoding
  end

  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {capabilities: Capability.t list}

    val encoding : t encoding
  end
end

module Device_lists : sig
  type%accessor t = {changed: string list option; left: string list option}

  val encoding : t encoding
end

module Devices : sig
  module Device : sig
    type%accessor t = {
      user_id: string option;
      (* Once again not advertised in the documentation *)
      device_id: string;
      display_name: string option;
      last_seen_ip: string option;
      last_seen_ts: int option;
    }

    val encoding : t encoding
  end

  module List : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {devices: Device.t list option}

      val encoding : t encoding
    end
  end

  module Get : sig
    module Query : Empty.QUERY
    module Response = Device
  end

  module Put : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {display_name: string option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Delete : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {auth: Authentication.t option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Delete_list : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {devices: string list; auth: Authentication.t option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end
end

module Jwk : sig
  type%accessor t = {
    kty: string;
    key_ops: string list;
    alg: string;
    k: string;
    ext: bool;
  }

  val encoding : t encoding
end

module Encrypted_file : sig
  type%accessor t = {
    url: string;
    key: Jwk.t;
    iv: string;
    hashes: (string * string) list;
    v: string;
  }

  val encoding : t encoding
end

module Errors : sig
  module Error : sig
    type%accessor t = {errcode: string; error: string option}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Rate_limited : sig
    type%accessor t = {errcode: string; error: string; retry_after_ms: int option}

    val encoding : t encoding
    val pp : t Fmt.t
  end

  module Auth_error : sig
    module Flow : sig
      type%accessor t = {stages: string list}

      val encoding : t encoding
      val pp : t Fmt.t
    end

    type%accessor t = {
      errcode: string option;
      error: string option;
      completed: string list option;
      flows: Flow.t list;
      params: (string * (string * string) list) list option;
      session: string option;
    }

    val encoding : t encoding
    val pp : t Fmt.t
  end

  type t = Error of Error.t | Auth_error of Auth_error.t | Rate_limited of Rate_limited.t

  val encoding : t encoding
  val pp : t Fmt.t
end

module Thumbnail_info : sig
  type%accessor t = {
    h: int option;
    w: int option;
    mimetype: string option;
    size: int option;
  }

  val encoding : t encoding
end

module Image_info : sig
  type%accessor t = {
    h: int option;
    w: int option;
    mimetype: string option;
    size: int option;
    thumbnail_url: string option;
    thumbnail_file: Encrypted_file.t option;
    thumbnail_info: Thumbnail_info.t option;
  }

  val encoding : t encoding
end

module File_info : sig
  type%accessor t = {
    mimetype: string option;
    size: int option;
    thumbnail_url: string option;
    thumbnail_file: Encrypted_file.t option;
    thumbnail_info: Thumbnail_info.t option;
  }

  val encoding : t encoding
end

module Location_info : sig
  type%accessor t = {
    thumbnail_url: string option;
    thumbnail_file: Encrypted_file.t option;
    thumbnail_info: Thumbnail_info.t option;
  }

  val encoding : t encoding
end

module Video_info : sig
  type%accessor t = {
    duration: int option;
    h: int option;
    w: int option;
    mimetype: string option;
    size: int option;
    thumbnail_url: string option;
    thumbnail_file: Encrypted_file.t option;
    thumbnail_info: Thumbnail_info.t option;
  }

  val encoding : t encoding
end

module Context : sig
  module Query : sig
    type%accessor t = {limit: int option}

    val args : t -> (string * string list) list
  end

  module Response : sig
    type%accessor t = {
      start: string option;
      end_: string option;
      events_before: Events.Room_event.t list option;
      event: Events.Room_event.t option;
      events_after: Events.Room_event.t list option;
      state: Events.State_event.t list option;
    }

    val encoding : t encoding
  end
end

module Filter : sig
  module Event_filter : sig
    type%accessor t = {
      limit: int option;
      not_senders: string list option;
      not_types: string list option;
      senders: string list option;
      types: string list option;
    }

    val encoding : t encoding
  end

  module State_filter : sig
    type%accessor t = {
      limit: int option;
      not_senders: string list option;
      not_types: string list option;
      senders: string list option;
      types: string list option;
      lazy_load_members: bool option;
      include_redundant_members: bool option;
      not_rooms: string list option;
      rooms: string list option;
      contains_url: bool option;
    }

    val encoding : t encoding
  end

  module Room_event_filter = State_filter

  module Room_filter : sig
    type%accessor t = {
      not_rooms: string list option;
      rooms: string list option;
      ephemeral: Room_event_filter.t option;
      include_leave: bool option;
      state: Room_event_filter.t option;
      timeline: Room_event_filter.t option;
      account_data: Room_event_filter.t option;
    }

    val encoding : t encoding
  end

  module Filter : sig
    module Event_format : sig
      type t = Client | Federation

      val encoding : t encoding
    end

    type%accessor t = {
      event_fields: string list option;
      event_format: Event_format.t option;
      presence: Event_filter.t option;
      account_data: Event_filter.t option;
      room: Room_filter.t option;
    }

    val encoding : t encoding
  end

  module Post : sig
    module Query : Empty.QUERY
    module Request = Filter

    module Response : sig
      type%accessor t = {filter_id: string}

      val encoding : t encoding
    end
  end

  module Get : sig
    module Query : Empty.QUERY
    module Response = Filter
  end
end

module Fully_read : sig
  module Query : Empty.QUERY

  module Request : sig
    type%accessor t = {
      fully_read: string option;
      (* see fully_read.ml *)
      read: string option;
      hidden: bool option; (* not in the documentation *)
    }

    val encoding : t encoding
  end

  module Response : Empty.JSON
end

module Joined : sig
  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {joined: string list}

    val encoding : t encoding
  end
end

module Joining : sig
  module Invite : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {user_id: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Invite_thirdparty : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {id_server: string; medium: string; address: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Join_with_id : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {third_party_signed: unit option}

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {room_id: string}

      val encoding : t encoding
    end
  end

  module Join : sig
    module Query : sig
      type%accessor t = {server_name: string list option}

      val args : t -> (string * string list) list
    end

    module Request : sig
      type%accessor t = {third_party_signed: unit option}

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {room_id: string}

      val encoding : t encoding
    end
  end
end

module Key_event : sig
  module Verification : sig
    module Request : sig
      type%accessor t = {
        from_device: string;
        transaction_id: string;
        methods: string list;
        timestamp: int;
      }

      val encoding : t encoding
    end

    module Start : sig
      type%accessor t = {
        from_device: string;
        transaction_id: string;
        verification_method: string;
        next_method: string;
      }

      val encoding : t encoding
    end

    module Cancel : sig
      type%accessor t = {transaction_id: string; reason: string; code: string}

      val encoding : t encoding
    end
  end

  module Sas_verification : sig
    module Sas : sig
      type t = Decimal | Emoji

      val encoding : t encoding
    end

    module Start : sig
      type%accessor t = {
        from_device: string;
        transaction_id: string;
        verification_method: string;
        key_agreement_protocols: string list;
        hashes: string list;
        message_authentication_codes: string list;
        short_authentication_string: Sas.t list;
      }

      val encoding : t encoding
    end

    module Accept : sig
      type%accessor t = {
        transaction_id: string;
        verification_method: string;
        key_agreement_protocol: string;
        hash: string;
        message_authentication_code: string;
        short_authentication_string: Sas.t list;
        commitment: string;
      }

      val encoding : t encoding
    end

    module Key : sig
      type%accessor t = {transaction_id: string; key: string}

      val encoding : t encoding
    end

    module Mac : sig
      type%accessor t = {
        transaction_id: string;
        mac: (string * string) list;
        keys: string;
      }

      val encoding : t encoding
    end
  end
end

module Keys : sig
  module Upload : sig
    module Query : Empty.QUERY

    module Request : sig
      module Device_keys : sig
        type%accessor t = {
          user_id: string;
          device_id: string;
          algorithms: string list;
          keys: (string * string) list;
          signatures: (string * (string * string) list) list;
        }

        val encoding : t encoding
      end

      module Keys_format : sig
        type t = Key of string | Object_key of Ezjsonm.value

        val encoding : t encoding
      end

      type%accessor t = {
        device_keys: Device_keys.t option;
        one_time_keys: (string * Keys_format.t) list option;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {one_time_key_counts: (string * int) list}

      val encoding : t encoding
    end
  end

  module Query : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        timeout: int option;
        device_keys: (string * string list) list;
        token: string option;
      }

      val encoding : t encoding
    end

    module Response : sig
      module Device_keys : sig
        module Unsigned_device_info : sig
          type%accessor t = {device_display_name: string option}

          val encoding : t encoding
        end

        type%accessor t = {
          user_id: string;
          device_id: string;
          algorithms: string list;
          keys: (string * string) list;
          signatures: (string * (string * string) list) list;
          unsigned: Unsigned_device_info.t option;
        }

        val encoding : t encoding
      end

      type%accessor t = {
        failures: (string * Ezjsonm.value) list option;
        device_keys: (string * (string * Device_keys.t) list) list option;
      }

      val encoding : t encoding
    end
  end

  module Claim : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        timeout: int;
        one_time_keys: (string * (string * string) list) list;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {
        failures: (string * Ezjsonm.value) list;
        one_time_keys: (string * (string * string) list) list; (* to correct *)
      }

      val encoding : t encoding
    end
  end

  module Changes : sig
    module Query : sig
      type%accessor t = {from: string; _to: string}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {changed: string list option; left: string list option}

      val encoding : t encoding
    end
  end
end

module Leaving : sig
  module Leave : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end

  module Forget : sig
    module Request : Empty.JSON
    module Response : Empty.JSON
  end

  module Kick : sig
    module Request : sig
      type%accessor t = {reason: string option; user_id: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end
end

module Well_known : sig
  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {homeserver: string; identity_server: string option}

    val encoding : t encoding
    val pp : Format.formatter -> t -> unit
  end
end

module Login : sig
  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {types: string list option}

      val encoding : t encoding
      val pp : t Fmt.t
    end
  end

  module Post : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        auth: Authentication.t;
        device_id: string option;
        initial_device_display_name: string option;
      }

      val encoding : t encoding
      val pp : t Fmt.t
    end

    module Response : sig
      type%accessor t = {
        user_id: string option;
        access_token: string option;
        home_server: string option;
        device_id: string option;
        well_known: Well_known.Response.t option;
      }

      val encoding : t encoding
      val pp : t Fmt.t
    end
  end
end

module Logout : sig
  module Logout : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end

  module Logout_all : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end
end

module Media : sig
  val raw : Ezjsonm.value encoding

  module Upload : sig
    module Query : sig
      type%accessor t = {filename: string option}

      val args : t -> (string * string list) list

      module Header : sig
        type%accessor t = {content_type: string; content_length: int}

        val header : t -> (string * string) list
      end
    end

    module Request : sig
      type%accessor t = {file: string}

      val to_string : t -> string
    end

    module Response : sig
      type%accessor t = {content_uri: string}

      val encoding : t encoding
    end
  end

  module Download : sig
    module Query : sig
      type%accessor t = {allow_remote: bool option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {file: string}

      val of_string : string -> t
    end
  end

  module Download_filename : sig
    module Response = Download.Response
    module Query = Download.Query
  end

  module Thumbnail : sig
    module Query : sig
      module Rezising : sig
        type t = Crop | Scale
      end

      type%accessor t = {
        width: int;
        height: int;
        rezising_method: Rezising.t option;
        allow_remote: bool option;
      }

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {thumbnail: string}

      val of_string : string -> t
    end
  end

  module Preview : sig
    module Query : sig
      type%accessor t = {url: string; ts: int option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {infos: (string * Ezjsonm.value) list}

      val encoding : t encoding
    end
  end

  module Config : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {upload_size: int option}

      val encoding : t encoding
    end
  end
end

module Notifications : sig
  module Query : sig
    type%accessor t = {
      from: string option;
      limit: int option;
      only: bool option;
    }

    val args : t -> (string * string list) list
  end

  module Response : sig
    module Notification : sig
      type%accessor t = {
        actions: Push_rule.Action.t;
        event: Events.Room_event.t;
        profile_tag: string option;
        read: bool;
        room_id: string;
        ts: int;
      }

      val encoding : t encoding
    end

    type%accessor t = {
      next_token: string option option;
      notifications: Notification.t list;
    }

    val encoding : t encoding
  end
end

module Open_id : sig
  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {
      access_token: string;
      token_type: string;
      matrix_server_name: string;
      expires_in: int;
    }

    val encoding : t encoding
  end
end

module Presence : sig
  module Put : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        presence: Events.Event_content.Presence.Presence.t;
        status_msg: string option;
      }

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {
        presence: Events.Event_content.Presence.Presence.t;
        last_active_ago: int option;
        status_msg: string option;
        currently_active: bool option;
        user_id: string option;
      }

      val encoding : t encoding
    end
  end
end

module Preview : sig
  module Query : sig
    type%accessor t = {
      from: string option;
      timeout: int option;
      room_id: string option;
    }

    val args : t -> (string * string list) list
  end

  module Response : sig
    type%accessor t = {
      start: string option;
      end_: string option;
      chunk: Events.Room_event.t list option;
    }

    val encoding : t encoding
  end
end

module Profile : sig
  module Display_name : sig
    module Set : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {displayname: string option}

        val encoding : t encoding
      end

      module Response : Empty.JSON
    end

    module Get : sig
      module Query : Empty.QUERY

      module Response : sig
        type%accessor t = {displayname: string option}

        val encoding : t encoding
      end
    end
  end

  module Avatar_url : sig
    module Set : sig
      module Query : Empty.QUERY

      module Request : sig
        type%accessor t = {avatar_url: string option}

        val encoding : t encoding
      end

      module Response : Empty.JSON
    end

    module Get : sig
      module Query : Empty.QUERY

      module Response : sig
        type%accessor t = {avatar_url: string option}

        val encoding : t encoding
      end
    end
  end

  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {displayname: string option; avatar_url: string option}

      val encoding : t encoding
    end
  end
end

module Push_rules : sig
  module Kind : sig
    type t = Override | Underride | Sender | Room | Content
  end

  module Get_all : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {
        content: Push_rule.t list option;
        override: Push_rule.t list option;
        room: Push_rule.t list option;
        sender: Push_rule.t list option;
        underride: Push_rule.t list option;
      }

      val encoding : t encoding
    end
  end

  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {push_rules: Push_rule.t}

      val encoding : t encoding
    end
  end

  module Delete : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end

  module Put : sig
    module Query : sig
      type%accessor t = {before: string option; after: string option}

      val args : t -> (string * string list) list
    end

    module Request : sig
      module Action : sig
        type t = Notify | Dont_notify | Coalesce | Set_weak

        val encoding : t encoding
      end

      type%accessor t = {
        actions: Action.t list;
        conditions: Push_rule.Push_condition.t list;
        pattern: string;
      }

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get_enabled : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {enabled: bool}

      val encoding : t encoding
    end
  end

  module Set_enabled : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {enabled: bool}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get_actions : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {actions: string list}

      val encoding : t encoding
    end
  end

  module Set_actions : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {actions: string list}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end
end

module Pushers : sig
  module Pusher : sig
    module Pusher_data : sig
      type%accessor t = {url: string option; format: string option}

      val encoding : t encoding
    end

    type%accessor t = {
      pushkey: string;
      kind: string;
      app_id: string;
      app_display_name: string;
      device_display_name: string;
      profile_tag: string option;
      lang: string;
      data: Pusher_data.t;
    }

    val encoding : t encoding
  end

  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {pushers: Pusher.t list option}

      val encoding : t encoding
    end
  end

  module Set : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {pusher: Pusher.t; append: bool option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end
end

module Receipt : sig
  module Query : Empty.QUERY
  module Request : Empty.JSON
  module Response : Empty.JSON
end

module Register : sig
  module Register : sig
    module Query : sig
      module Kind : sig
        type t = User | Guest
      end

      type%accessor t = {kind: Kind.t option}

      val args : t -> (string * string list) list
    end

    module Request : sig
      type%accessor t = {
        auth: Authentication.t option;
        bind_email: bool option;
        bind_msisdn: bool option;
        username: string option;
        password: string option;
        device_id: string option;
        initial_device_display_name: string option;
        inhibit_login: bool option;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {
        user_id: string;
        access_token: string option;
        home_server: string;
        device_id: string option;
      }

      val encoding : t encoding
    end
  end

  module Available : sig
    module Query : sig
      type%accessor t = {username: string}

      val args : t -> (string * string list) list
    end

    module Request : Empty.JSON

    module Response : sig
      type%accessor t = {available: bool}

      val encoding : t encoding
    end
  end

  module Email_request_token : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        client_secret: string;
        email: string;
        send_attempt: int;
        next_link: string option;
        id_server: string;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {
        success: bool option;
        sid: string;
        submit_url: string option;
      }

      val encoding : t encoding
    end
  end

  module Msisdn_request_token : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {
        client_secret: string;
        country: string;
        phone_number: string;
        send_attempt: int;
        next_link: string option;
        id_server: string;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {
        success: bool option;
        sid: string;
        submit_url: string option;
        msisdn: string option;
        intl_fmt: string option;
      }

      val encoding : t encoding
    end
  end
end

module Report : sig
  module Query : Empty.QUERY

  module Request : sig
    type%accessor t = {score: int; reason: string}

    val encoding : t encoding
  end

  module Response : Empty.JSON
end

module Room_event : sig
  module Get : sig
    module Event : sig
      module Query : Empty.QUERY
      module Request : Empty.JSON
      module Response = Events.Room_event
    end

    module State_key : sig
      module Query : Empty.QUERY
      module Request : Empty.JSON

      module Response : sig
        type t = Ezjsonm.value

        val encoding : t encoding
      end
    end

    module State : sig
      module Query : Empty.QUERY
      module Request : Empty.JSON

      module Response : sig
        type%accessor t = {events: Events.State_event.t list}

        val encoding : t encoding
      end
    end

    module Members : sig
      module Query : sig
        type%accessor t = {
          at: string option;
          membership: Events.Event_content.Membership.t option;
          not_membership: Events.Event_content.Membership.t option;
        }

        val args : t -> (string * string list) list
      end

      module Request : Empty.JSON

      module Response : sig
        type%accessor t = {chunk: Events.State_event.t list}

        val encoding : t encoding
      end
    end

    module Joined_members : sig
      module Query : Empty.QUERY
      module Request : Empty.JSON

      module Response : sig
        module User : sig
          type%accessor t = {
            display_name: string option;
            avatar_url: string option;
          }

          val encoding : t encoding
        end

        type%accessor t = {joined: (string * User.t) list option}

        val encoding : t encoding
      end
    end
  end

  module Put : sig
    module State_event : sig
      module Query = Empty.Query

      module Request : sig
        type%accessor t = {event: Events.Event_content.t}

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {event_id: string}

        val encoding : t encoding
      end
    end

    module Message_event : sig
      module Query = Empty.Query

      module Request : sig
        type%accessor t = {event: Events.Event_content.Message.t}

        val encoding : t encoding
      end

      module Response : sig
        type%accessor t = {event_id: string}

        val encoding : t encoding
      end
    end
  end
end

module Room : sig
  module Visibility : sig
    type t = Public | Private

    val encoding : t encoding
  end

  module Create : sig
    module Query : Empty.QUERY

    module Request : sig
      module Invite_3pid : sig
        type%accessor t = {id_server: string; medium: string; addresss: string}

        val encoding : t encoding
      end

      module Preset : sig
        type t = Public | Private | Trusted_private

        val encoding : t encoding
      end

      type%accessor t = {
        visibility: Visibility.t option;
        room_alias_name: string option;
        name: string option;
        topic: string option;
        invite: string list option;
        invite_3pid: Invite_3pid.t list option;
        room_version: string option;
        creation_content: Events.Event_content.Create.t option;
        initial_state: Events.State_event.t list option;
        preset: Preset.t option;
        is_direct: bool option;
        power_level_content_override: Events.Event_content.Power_levels.t option;
      }

      val encoding : t encoding
    end

    module Response : sig
      type%accessor t = {room_id: string}

      val encoding : t encoding
    end
  end

  module Create_alias : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {room_id: string}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Resolve_alias : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON

    module Response : sig
      type%accessor t = {room_id: string option; servers: string list option}

      val encoding : t encoding
    end
  end

  module Delete_alias : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end
end

module Room_listing : sig
  module Get_visibility : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {visibility: Room.Visibility.t}

      val encoding : t encoding
    end
  end

  module Set_visibility : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {visibility: Room.Visibility.t option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Get_public_rooms : sig
    module Query : sig
      type%accessor t = {
        limit: int option;
        since: string option;
        server: string option;
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
    module Query : sig
      type%accessor t = {server: string option}

      val args : t -> (string * string list) list
    end

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

module Rooms : sig
  module Room_summary : sig
    type%accessor t = {
      heroes: string list option;
      joined_member_count: int option;
      invited_member_count: int option;
    }

    val encoding : t encoding
  end

  module Timeline : sig
    type%accessor t = {
      events: Events.Room_event.t list option;
      limited: bool option;
      prev_batch: string option;
    }

    val encoding : t encoding
  end

  module Joined_room : sig
    module Unread_notifications : sig
      type%accessor t = {
        highlight_count: int option;
        notification_count: int option;
      }

      val encoding : t encoding
    end

    type%accessor t = {
      summary: Room_summary.t option;
      state: Events.State_event.t list option;
      timeline: Timeline.t option;
      ephemeral: Events.Event.t list option;
      account_data: Events.Event.t list option;
      unread_notifications: Unread_notifications.t option;
    }

    val encoding : t encoding
  end

  module Invited_room : sig
    type%accessor t = {invite_state: Events.State_event.t list option}

    val encoding : t encoding
  end

  module Left_room : sig
    type%accessor t = {
      state: Events.State_event.t list option;
      timeline: Timeline.t option;
      account_data: Events.Room_event.t list option;
    }

    val encoding : t encoding
  end

  type%accessor t = {
    join: (string * Joined_room.t) list;
    invite: (string * Invited_room.t) list;
    leave: (string * Left_room.t) list;
  }

  val encoding : t encoding
end

module Search : sig
  module Query : sig
    type%accessor t = {next_batch: string option}

    val args : t -> (string * string list) list
  end

  module Request : sig
    module Criteria : sig
      module Key : sig
        type t = Content_body | Content_name | Content_topic

        val encoding : t encoding
      end

      module Filter : sig
        type%accessor t = {
          limit: int option;
          not_senders: string list option;
          not_types: string list option;
          senders: string list option;
          types: string list option;
          lazy_load_members: bool option;
          include_redundant_members: bool option;
          not_rooms: string list option;
          rooms: string list option;
          contains_url: bool option;
        }

        val encoding : t encoding
      end

      module Order : sig
        type t = Recent | Rank

        val encoding : t encoding
      end

      module Include_event_context : sig
        type%accessor t = {
          before_limit: int option;
          after_limit: int option;
          include_profile: bool option;
        }

        val encoding : t encoding
      end

      module Groupings : sig
        module Group : sig
          type t = Room_id | Sender

          val encoding : t encoding
        end

        type%accessor t = {group_by: Group.t list option}

        val encoding : t encoding
      end

      type%accessor t = {
        search_term: string;
        keys: Key.t option;
        filter: Filter.t option;
        order_by: Order.t option;
        event_context: Include_event_context.t option;
        include_state: bool option;
        groupings: Groupings.t option;
      }

      val encoding : t encoding
    end

    type%accessor t = {criterias: Criteria.t option}

    val encoding : t encoding
  end

  module Response : sig
    module Results : sig
      module Result : sig
        module Event_context : sig
          module User_profile : sig
            type%accessor t = {
              displayname: string option;
              avatar_url: string option;
            }

            val encoding : t encoding
          end

          type%accessor t = {
            start: string option;
            end_: string option;
            profile_info: (string * User_profile.t) list option;
            events_before: Events.Room_event.t list option;
            events_after: Events.Room_event.t list option;
          }

          val encoding : t encoding
        end

        type%accessor t = {
          rank: int option;
          result: Events.Room_event.t option;
          context: Event_context.t option;
        }

        val encoding : t encoding
      end

      module Group_value : sig
        type%accessor t = {
          next_batch: string option;
          order: int option;
          results: string list option;
        }

        val encoding : t encoding
      end

      type%accessor t = {
        count: int option;
        highlights: string list option;
        results: Result.t list option;
        state: (string * Events.State_event.t) list option;
        groups: (string * (string * Group_value.t) list) list option;
        next_batch: string option;
      }

      val encoding : t encoding
    end

    type%accessor t = {results: Results.t option}

    val encoding : t encoding
  end
end

module Send_to_device : sig
  module Query : Empty.QUERY

  module Request : sig
    type%accessor t = {
      messages: (string * (string * Ezjsonm.value) list) list option;
    }

    val encoding : t encoding
  end

  module Response : Empty.JSON
end

module Session_data : sig
  type%accessor t = {
    algorithm: string;
    forwarding_curve25519_key_chain: string list;
    room_id: string;
    sender_key: string;
    sender_claimed_keys: (string * string) list;
    session_id: string;
    session_key: string;
  }

  val encoding : t encoding
end

module Sso : sig
  module Query : sig
    type%accessor t = {redirect_url: string}

    val args : t -> (string * string list) list
  end

  module Response : Empty.JSON
end

module Sync : sig
  module Query : sig
    module Presence : sig
      type t = Offline | Online | Unavailable

      val pp : Format.formatter -> t -> unit
    end

    type%accessor t = {
      filter: string option;
      since: string option;
      full_state: bool option;
      set_presence: Presence.t option;
      timeout: int option;
    }

    val args : t -> (string * string list) list
    val pp : t Fmt.t
  end

  module Response : sig
    type%accessor t = {
      next_batch: string;
      rooms: Rooms.t option;
      presence: Events.State_event.t list option;
      account_data: Events.State_event.t list option;
      to_device: Events.State_event.t list option;
      device_lists: Device_lists.t option;
      device_one_time_keys_count: (string * int) list option;
      groups: Ezjsonm.value option; (* Not on the documentation*)
    }

    val encoding : t encoding
    val pp : t Fmt.t
  end
end

module Tag : sig
  module Get : sig
    module Query : Empty.QUERY

    module Response : sig
      module Tag : sig
        type%accessor t = {order: float option}

        val encoding : t encoding
      end

      type%accessor t = {tags: (string * Tag.t) list}

      val encoding : t encoding
    end
  end

  module Put : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {order: float option}

      val encoding : t encoding
    end

    module Response : Empty.JSON
  end

  module Delete : sig
    module Query : Empty.QUERY
    module Request : Empty.JSON
    module Response : Empty.JSON
  end
end

module Third_party_network : sig
  module Protocol : sig
    module Field_type : sig
      type%accessor t = {regexp: string; placeholder: string}

      val encoding : t encoding
    end

    module Instance : sig
      type%accessor t = {
        desc: string;
        icon: string option;
        fields: (string * string) list;
        network_id: string;
      }

      val encoding : t encoding
    end

    type%accessor t = {
      user_fields: string list;
      location_fields: string list;
      icon: string;
      field_types: (string * Field_type.t) list;
      instances: Instance.t list;
    }

    val encoding : t encoding
  end

  module Location : sig
    type%accessor t = {
      alias: string;
      protocol: string;
      fields: (string * string) list;
    }

    val encoding : t encoding
  end

  module User : sig
    type%accessor t = {
      user_id: string;
      protocol: string;
      fields: (string * string) list;
    }

    val encoding : t encoding
  end

  module Protocols : sig
    module Query : Empty.QUERY

    module Response : sig
      type%accessor t = {protocols: (string * Protocol.t) list}

      val encoding : t encoding
    end
  end

  module Get_protocol : sig
    module Query : Empty.QUERY
    module Response = Protocol
  end

  module Get_location : sig
    module Query : sig
      type%accessor t = {search_fields: string option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {locations: Location.t list}

      val encoding : t encoding
    end
  end

  module Get_user : sig
    module Query : sig
      type%accessor t = {fields: (string * string list) list option}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {users: User.t list}

      val encoding : t encoding
    end
  end

  module Location_from_alias : sig
    module Query : sig
      type%accessor t = {alias: string}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {locations: Location.t list}

      val encoding : t encoding
    end
  end

  module User_from_user_id : sig
    module Query : sig
      type%accessor t = {user_id: string}

      val args : t -> (string * string list) list
    end

    module Response : sig
      type%accessor t = {users: User.t list}

      val encoding : t encoding
    end
  end
end

module Typing : sig
  module Query : Empty.QUERY

  module Request : sig
    type%accessor t = {typing: bool; timeout: int option}

    val encoding : t encoding
  end

  module Response : Empty.JSON
end

module Upgrade : sig
  module Query : Empty.QUERY

  module Request : sig
    type%accessor t = {new_version: string}

    val encoding : t encoding
  end

  module Response : sig
    type%accessor t = {replacement_room: string}

    val encoding : t encoding
  end
end

module User_directory : sig
  module Search : sig
    module Query : Empty.QUERY

    module Request : sig
      type%accessor t = {search_term: string; limited: int option}

      val encoding : t encoding
    end

    module Response : sig
      module User : sig
        type%accessor t = {
          user_id: string;
          display_name: string option;
          avatar_url: string option;
        }

        val encoding : t encoding
      end

      type%accessor t = {results: User.t list; limited: bool}

      val encoding : t encoding
    end
  end
end

module Versions : sig
  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {
      versions: string list;
      unstable_features: (string * bool) list option;
    }

    val encoding : t encoding
  end
end

module Voip : sig
  module Query : Empty.QUERY

  module Response : sig
    type%accessor t = {
      username: string option;
      password: string option;
      uris: string list option;
      ttl: int option;
    }

    val encoding : t encoding
  end
end

module Whois : sig
  module Query : Empty.QUERY

  module Response : sig
    module Device_info : sig
      module Session_info : sig
        module Connection_info : sig
          type%accessor t = {
            ip: string option;
            last_seen: int option;
            user_agent: string option;
          }

          val encoding : t encoding
        end

        type%accessor t = {connections: Connection_info.t list option}

        val encoding : t encoding
      end

      type%accessor t = {sessions: Session_info.t list option}

      val encoding : t encoding
    end

    type%accessor t = {
      user_id: string option;
      devices: (string * Device_info.t) list option;
    }

    val encoding : t encoding
  end
end
