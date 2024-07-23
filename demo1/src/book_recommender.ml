open! Core

module State = struct
  type t =
    { mutable visited : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; recommendations : Book.t list
    }
  [@@deriving sexp_of]
end

(* user said yes to X book, so now we call this on all tis subjects : *)
let update_to_visit_from_subject ~(state : State.t) ~subject =
  let visited = state.visited in
  let to_visit = state.to_visit in
  let books_raw = Book_fetch.Fetcher.Subjects.fetch_sub subject in
  let books = Page_parser.Subject_page.parse_books books_raw in
  List.iter books ~f:(fun (book : Book.t) ->
    let key = Book.key book in
    book.heuristic <- book.heuristic + 1;
    if not
         (List.exists visited ~f:(fun k -> equal 0 (Book.Key.compare k key)))
    then Book.Binary_heap.add to_visit book)
;;

let%expect_test "Get books from subject: Tooth Fairy" =
  let dummy =
    Book.create ~title:"Dummy" ~key:"Key" ~subjects:[] ~isbn:(Some 1)
  in
  let state =
    { State.visited = []
    ; to_visit = Book.Binary_heap.create ~dummy 1
    ; recommendations = []
    }
  in
  update_to_visit_from_subject ~state ~subject:"tooth_fairy";
  Book.Binary_heap.iter (fun book -> Book.print book) state.to_visit
;;

let get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  state.visited <- List.append state.visited [ Book.key new_book ];
  new_book
;;

let%expect_test "Get next book from Tooth Fairy subject original queue" =
  let dummy =
    Book.create ~title:"Dummy" ~key:"Key" ~subjects:[] ~isbn:(Some 1)
  in
  let state =
    { State.visited = []
    ; to_visit = Book.Binary_heap.create ~dummy 1
    ; recommendations = []
    }
  in
  update_to_visit_from_subject ~state ~subject:"tooth_fairy";
  let next_book = get_next_book ~state in
  Book.print next_book
;;

let _calculate_heuristic = ()
(* to implement and call in update_to_visit*)
