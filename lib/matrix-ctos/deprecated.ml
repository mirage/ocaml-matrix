module Events = struct let path = "/_matrix/client/r0/events" end
module Initial_sync = struct let path = "/_matrix/client/r0/initialSync" end

module Events_events_id = struct
  let path = "/_matrix/client/r0/events/{eventId}"
end
