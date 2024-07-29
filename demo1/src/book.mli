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

module Author : sig
  type t = string [@@deriving sexp, compare, hash]
end

module Description : sig
  type t = string [@@deriving sexp, compare, hash]
end

module Publish_Date : sig
  type t = int [@@deriving sexp, compare, hash]
end

type t =
  { title : Title.t
  ; author : Author.t option
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : float
  ; mutable description : string
  ; publish_date : Publish_Date.t option
  }
[@@deriving sexp, compare]

(* creates a new book and initilizes the heuristic to 0*)
val create
  :  title:string
  -> author:string option
  -> key:string
  -> subjects:string list
  -> isbn:int option
  -> publish_date:int option
  -> t

(* prints the title and Open Library key of a book*)
val print : t -> unit
val author : t -> Author.t option
val description : t -> Description.t
val isbn : t -> Isbn.t option
val subjects : t -> Subject.t list
val key : t -> Key.t
val title : t -> Title.t
val heuristic : t -> float

module Binary_heap : Binary_heap.S with type Value.t = t
