open Matrix_common

module Query = Empty.Query

let path room_id receipt_type event_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/receipt/" ^ receipt_type ^ "/" ^ event_id

module Request = Empty.Json

module Response = Empty.Json

let needs_auth = true
