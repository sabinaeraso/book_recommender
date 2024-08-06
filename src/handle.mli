open! Core
open Async

(* They are not interested. Add the current book to visited and update the
   state with these new lists. Generate a new book and print it out. *)
val handle_no : state:Book_recommender.State.t -> unit

(* They enjoyed this book! Add the current book to visited and update the
   state with these new lists. Now, get every book in every subject of this
   book and add it to the to_visit heap. Once your to_visit is updated,
   generate a new book and print it out. *)
val handle_read_yes
  :  float
  -> state:Book_recommender.State.t
  -> unit Deferred.t

(* this adds the book to recommendations and calls handle_read_yes to update
   the heap with the books in the subjects of this book*)
val handle_yes : float -> state:Book_recommender.State.t -> unit Deferred.t

(* They dont want to see any more books. Print out their recommended books.*)
val handle_done : state:Book_recommender.State.t -> unit
