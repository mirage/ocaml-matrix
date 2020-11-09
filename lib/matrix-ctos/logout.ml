open Matrix_common

module Logout =
struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/logout"

  module Request = Empty.Json

  module Response = Empty.Json

  let needs_auth = true
end

module Logout_all =
struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/logout/all"

  module Request = Empty.Json

  module Response = Empty.Json

  let needs_auth = true
end
