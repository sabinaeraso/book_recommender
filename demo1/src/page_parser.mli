open! Core

module Subject_page : sig
  val parse_books : string -> Book.t list
end

module Book_page : sig
  val parse_book : string -> Book.t
  val get_book_description : string -> string
end

module Search_page : sig
  val parse_searches : string -> Book.t
end

val command : Command.t
