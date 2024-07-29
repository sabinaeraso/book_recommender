open! Core
module Isbn = Int [@@deriving sexp, compare, hash]
module Key = String [@@deriving sexp, compare, hash]
module Title = String [@@deriving sexp, compare, hash]
module Subject = String [@@deriving sexp, compare, hash]
module Author = String [@@deriving sexp, compare, hash]
module Description = String [@@deriving sexp, compare, hash]

type t =
  { title : Title.t
  ; author : Author.t option
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : float
  ; mutable description : Description.t
  }
[@@deriving sexp, compare]

let print book = Core.printf !{|Title: %{sexp:Title.t}
|} book.title

let create ~title ~author ~key ~subjects ~isbn =
  { title; author; key; subjects; isbn; heuristic = 10.0; description = "" }
;;

let compare_by_heuristic =
  Comparable.lift Float.compare ~f:(fun book -> book.heuristic)
;;

let subjects t = t.subjects
let key t = t.key
let isbn t = t.isbn
let title t = t.title
let heuristic t = t.heuristic
let author t = t.author
let description t = t.description

module T = struct
  type nonrec t = t [@@deriving sexp]

  let compare = compare_by_heuristic
  let key = key
end

module Binary_heap = Binary_heap.Make (T)
