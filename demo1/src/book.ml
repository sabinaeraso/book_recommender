open! Core
module Isbn = Int [@@deriving sexp, compare, hash]
module Google_Id = String [@@deriving sexp, compare, hash]
module OL_Id = String [@@deriving sexp, compare, hash]
module Title = String [@@deriving sexp, compare, hash]
module Subject = String [@@deriving sexp, compare, hash]
module Author = String [@@deriving sexp, compare, hash]
module Description = String [@@deriving sexp, compare, hash]
module Publish_Date = Int [@@deriving sexp, compare, hash]

type t =
  { title : Title.t
  ; author : Author.t option
  ; ol_id : OL_Id.t
  ; mutable google_id : Google_Id.t
  ; isbn : Isbn.t option
  ; subjects : Subject.t list
  ; mutable heuristic : float
  ; mutable description : Description.t
  ; publish_date : Publish_Date.t option
  }
[@@deriving sexp, compare, fields ~getters]

let print book = Core.printf !{|Title: %{sexp:Title.t}
|} book.title

let create ~title ~author ~ol_id ~google_id ~isbn ~subjects ~publish_date =
  { title
  ; author
  ; ol_id
  ; google_id
  ; isbn
  ; subjects
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
  let key = google_id
end

module Binary_heap = Binary_heap.Make (T)
