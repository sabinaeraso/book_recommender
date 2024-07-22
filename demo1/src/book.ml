open! Core

(* information: title, id , subjects *)

module Id = Int
module Title = String
module Subject = String

type t =
  { title : Title.t
  ; id : Id.t
  ; subjects : Subject.t list
  }
[@@deriving sexp, compare, hash]

let to_string { title; id; _ } =
  Core.sprintf !{|Title: %{sexp:Title.t}
Id: %{sexp:Id.t}
|} title id
;;

let subjects t = t.subjects
let id t = t.id
let title t = t.title
