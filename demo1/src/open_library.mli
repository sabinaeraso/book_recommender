open! Core
open Async

module Fetch_and_parse : sig
  val get_books_from_subject : Cache.t -> string -> Book.t list Deferred.t
  val get_book_from_key : string -> Book.t
  val get_book_from_title : string -> Book.t
  val get_subjects_from_key : string -> string list
  val get_first_language_from_title : string -> string
end
