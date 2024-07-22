open! Core

module Isbn : sig
  type t = int [@@deriving sexp, compare, hash]
end

module Title : sig
  type t = String [@@deriving sexp, compare, hash]
end

module Key : sig
  type t = String [@@deriving sexp, compare, hash]
end

module Subject : sig
  type t = String [@@deriving sexp, compare, hash]
end

type t =
  { title : Title.t
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  }
[@@deriving sexp, compare, hash]

val create
  :  title:string
  -> key:string
  -> isbn:int option
  -> subjects:string
  -> t

val to_string : t -> string
val isbn : t -> Isbn.t option
val subjects : t -> Subject.t list
val key : t -> Key.t
val title : t -> Title.t
