open! Core

module Isbn : sig
  type t = int [@@deriving sexp, compare, hash]
end

module Title : sig
  type t = string [@@deriving sexp, compare, hash]
end

module OL_Id : sig
  type t = string [@@deriving sexp, compare, hash]
end

module Google_Id : sig
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
  ; ol_id : OL_Id.t
  ; google_id : Google_Id.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : float
  ; mutable description : string
  ; publish_date : Publish_Date.t option
  }
[@@deriving sexp, compare, fields ~getters]

(* creates a new book and initilizes the heuristic to 10*)
val create
  :  title:string
  -> author:string option
  -> ol_id:string
  -> google_id:string
  -> isbn:int option
  -> subjects:string list
  -> publish_date:int option
  -> t

(* prints the title *)
val print : t -> unit

module Binary_heap : Binary_heap.S with type Value.t = t
