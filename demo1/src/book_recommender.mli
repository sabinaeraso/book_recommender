open! Core

module State : sig
  type t =
    { mutable visited_books : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    ; mutable current_book : Book.t
    ; mutable visited_subjects : string list
    }
  [@@deriving sexp_of]

  val empty_state : t
end

(*takes in the current state and a new subject. adds all the books from this
  subject to the state's to_visit, along with an updated heuristic value.
  returns unit but updates the state that was passed*)
val update_to_visit_from_subject
  :  float
  -> state:State.t
  -> subject:string
  -> unit

(*takes in the current state and returns the next book to be returned by
  popping it off of the to_visit binary heap*)
val get_next_book : state:State.t -> Book.t
