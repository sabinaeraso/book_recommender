open! Core
module Isbn = Int
module Key = String
module Title = String
module Subject = String

type t =
  { title : Title.t
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  }
[@@deriving sexp, compare, hash]

let to_string book =
  Core.sprintf
    !{|Title: %{sexp:Title.t}
Key: %{sexp:Key.t}
|}
    book.title
    book.key
;;

let create ~title ~key ~subjects ~isbn = { title; key; subjects; isbn }
let subjects t = t.subjects
let key t = t.key
let isbn t = t.isbn
let title t = t.title
