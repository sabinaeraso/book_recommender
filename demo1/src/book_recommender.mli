open! Core

module State : sig
  type t =
    { mutable visited : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    }
  [@@deriving sexp_of]
end

val update_to_visit_from_subject : state:State.t -> subject:string -> unit
val get_next_book : state:State.t -> Book.t
