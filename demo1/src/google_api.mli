open! Core

module Fetcher : sig
  val fetch_book_by_id : string -> string
  val search_book_by_name : string -> string
end

module Parser : sig
  val get_book_id_from_search_json : string -> string
  val get_description_from_search_json : string -> string
  val get_authors_from_search_json : string -> string
  val get_self_link_from_search : string -> string
  val get_image_from_book : string -> string -> string
end

val command : Command.t
