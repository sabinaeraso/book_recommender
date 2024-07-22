open! Core

module Isbn : sig
  type t = int
end

module Title : sig
  type t = String
end

module Key : sig
  type t = String
end

module Subject : sig
  type t = String
end

type t [@@deriving sexp, compare, hash]

val to_string : t -> string
val isbn : t -> Isbn.t option
val subjects : t -> Subject.t list
val key : t -> Key.t
val title : t -> Title.t
