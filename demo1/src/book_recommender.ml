open! Core

module State = struct
  type t =
    { mutable visited : Book.Key.t list
         [@hash.ignore] (* some mutable set of books*)
    ; mutable to_visit : Binary_heap.Ordered.t [@hash.ignore]
    ; recommendations : Hashtbl.M(Book).t
    }
  [@@deriving compare, hash]

  let visited t = t.visited
  let to_visit t = t.to_visit
  let recommendations t = t.recommendations
end

let get_books_from_subject ~(state : State.t) ~subject =
  let visited = State.visited state in
  let to_visit = State.to_visit state in
  let recommendations = State.recommendations state in
  let books_raw = Fetcher.Subjects.fetch_sub subject in
  let books = Page_parser.Subject_page.get_works_list books_raw in
  List.iter books ~f:(fun (book : Book.t) ->
    let key = Book.key book in
    if not
         (List.exists visited ~f:(fun k -> equal 0 (Book.Key.compare k key)))
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
