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
let _get_books_from_subject ~(state : State.t) ~subject =
  let visited = state.visited in
  let to_visit = state.to_visit in
  let books_raw = Book_fetch.Fetcher.Subjects.fetch_sub subject in
  let books = Page_parser.Subject_page.parse_books books_raw in
  List.iter books ~f:(fun (book : Book.t) ->
    let key = Book.key book in
    if not
         (List.exists visited ~f:(fun k -> equal 0 (Book.Key.compare k key)))
    then Book.Binary_heap.add to_visit book)
;;

(* let%expect_test "Get books from subject" = let state = {State.visited =
   []; to_visit = Book.Binary_heap.create 200; recommendations = []} in
   get_books_from_subject ~state ~subject:"tooth_fairy";
   Book.Binary_heap.iter to_visit

   ;; *)

let _get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  state.visited <- List.append state.visited [ Book.key new_book ];
  new_book
;;
