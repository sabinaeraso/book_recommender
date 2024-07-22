open! Core

module Isbn : sig
  type t = int [@@deriving sexp, compare, hash]
end

module Title : sig
  type t = string [@@deriving sexp, compare, hash]
end

module Key : sig
  type t = string [@@deriving sexp, compare, hash]
end

module Subject : sig
  type t = string [@@deriving sexp, compare, hash]
end

type t =
  { title : Title.t
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; heuristic : int
  }
[@@deriving sexp, compare, hash]

val create
  :  title:string
  -> key:string
  -> subjects:string list
  -> isbn:int option
  -> heuristic:int
  -> t

val to_string : t -> string
val isbn : t -> Isbn.t option
val subjects : t -> Subject.t list
val key : t -> Key.t
val title : t -> Title.t
val heuristic : t -> int

module Binary_heap : Binary_heap.S with type Value.t = t
