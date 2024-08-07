open! Core

module Fetcher : sig
  val fetch_book_by_id : string -> string
  val search_book_by_name : string -> string
  val search_by_subject : ?maxResults:int -> string -> string
end

module Parser : sig
  val get_books_from_subject_search : string -> Book.t list
  val get_book_id_from_search_json : string -> string
  val get_description_from_search_json : string -> string
  val get_authors_from_search_json : string -> string
  val get_self_link_from_search : string -> string
  val get_categories_from_book : string -> string list
  val make_book_from_search : string -> Book.t
  val get_image_from_book : string -> image_size:string -> string
end

module Fetch_and_parse : sig
  val get_book_from_title : string -> Book.t
  val get_books_from_subject : string -> Book.t list
  val get_book_description_from_title : string -> string
  val get_categories_from_title : string -> string list
  val get_categories_from_id : string -> string list
  val get_cover_from_title : title:string -> image_size:string -> string
  val get_cover_from_id : id:string -> image_size:string -> string
  val get_authors_from_title : string -> string
end

val command : Command.t
