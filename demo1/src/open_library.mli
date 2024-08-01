open! Core

module Fetch_and_parse : sig
  val get_books_from_subject : string -> Book.t list
  val get_book_from_key : string -> Book.t
  val get_book_from_title : string -> Book.t
  val get_subjects_from_key : string -> string list
end
