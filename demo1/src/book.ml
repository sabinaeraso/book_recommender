open! Core
module Isbn = Int [@@deriving sexp, compare, hash]
module Key = String [@@deriving sexp, compare, hash]
module Title = String [@@deriving sexp, compare, hash]
module Subject = String [@@deriving sexp, compare, hash]
module Author = String [@@deriving sexp, compare, hash]
module Description = String [@@deriving sexp, compare, hash]
module Publish_Date = Int [@@deriving sexp, compare, hash]

type t =
  { title : Title.t
  ; author : Author.t option
  ; key : Key.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : float
  ; mutable description : Description.t
  ; publish_date : Publish_Date.t option
  }
[@@deriving sexp, compare, fields ~getters]

let print book = Core.printf !{|Title: %{sexp:Title.t}
|} book.title

let create ~title ~author ~key ~subjects ~isbn ~publish_date =
  { title
  ; author
  ; key
  ; subjects
  ; isbn
  ; heuristic = 10.0
  ; description = ""
  ; publish_date
  }
;;

let compare_by_heuristic =
  Comparable.lift Float.compare ~f:(fun book -> book.heuristic)
;;

module T = struct
  type nonrec t = t [@@deriving sexp]

  let compare = compare_by_heuristic
  let key = key
end

module Binary_heap = Binary_heap.Make (T)
