open! Core

val find_field : string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t

val find_field_option
  :  string
  -> (string * Yojson.Safe.t) list
  -> Yojson.Safe.t option

val format_field : string -> string

module Subject_page : sig
  val parse_books : string -> Book.t list
end

module Book_page : sig
  val parse_book : string -> Book.t
  val parse_subjects_from_book : string -> string list
  val get_book_description : string -> string
end

module Search_page : sig
  val parse_searches : string -> Book.t
  val get_first_language_from_json : string -> string
end

val command : Command.t
