open! Core

module Fetcher : sig
  module Subjects : sig
    val fetch_sub : ?limit:int -> string -> string
  end

  module Books : sig
    val fetch_key : string -> string
    val fetch_olid : string -> string
  end

  module Search_by_name : sig
    val fetch_from_search : string -> string
  end
end

val command : Command.t
