open! Core

module State = struct
  type t =
    { visited : int (* some mutable set of books*)
    ; to_visit : Binary_heap.Ordered.t
    ; recommendations : Hashtbl.M(Book).t
    }
end

let visited = Hashtbl.create (module Int)
let to_visit = Hashtbl.create (module Book)
let recommendations = Hashtbl.create (module Book)

let get_books_from_subject ~subject =
  let books = [] in
  (* file fetch subject to get list of books*)
  List.iter books ~f:(fun (book : Book.t) ->
    let isbn = Book.isbn book in
    if not (Hashtbl.mem visited isbn)
    then (
      match Hashtbl.add to_visit ~key:book ~data:0 with
      | `Ok -> Hashtbl.add_exn to_visit ~key:book ~data:1
      | `Duplicate ->
        Hashtbl.add_exn
          to_visit
          ~key:book
          ~data:(Hashtbl.find_exn to_visit book + 1)))
;;

let get_next_book ~visited ~to_visit ~recommendations =
  let new_book = Hashtbl.choose_randomly to_visit in
  ()
;;
