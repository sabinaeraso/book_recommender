open! Core
module Isbn = Int [@@deriving sexp, compare, hash]
module Key = String [@@deriving sexp, compare, hash]
module Title = String [@@deriving sexp, compare, hash]
module Subject = String [@@deriving sexp, compare, hash]

type t =
  { title : Title.t
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : int
  }
[@@deriving sexp, compare]

let print book =
  match book.isbn with
  | Some isbn ->
    printf
      !{|Title: %{sexp:Title.t}
Key: %{sexp:Key.t}
Isbn: %{sexp:Isbn.t}
|}
      book.title
      book.key
      isbn
  | None ->
    printf
      !{|Title: %{sexp:Title.t}
Key: %{sexp:Key.t}
|}
      book.title
      book.key
;;

let create ~title ~key ~subjects ~isbn =
  { title; key; subjects; isbn; heuristic = 0 }
;;

let compare_by_heuristic =
  Comparable.lift Int.compare ~f:(fun book -> book.heuristic)
;;

let subjects t = t.subjects
let key t = t.key
let isbn t = t.isbn
let title t = t.title
let heuristic t = t.heuristic

module T = struct
  type nonrec t = t [@@deriving sexp]

  let compare = compare_by_heuristic
end

module Binary_heap = Binary_heap.Make (T)
