open! Core
module Isbn = Int
module Title = String
module Key = String
module Subject = String

type t [@@deriving sexp, compare, hash]

val to_string : t -> string
val isbn : t -> Isbn.t
val subjects : t -> Subject.t list
val key : t -> Key.t
val title : t -> Title.t
