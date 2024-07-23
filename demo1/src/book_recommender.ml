open! Core

module State = struct
  type t =
    { mutable visited : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    ; mutable current_book : Book.t
    }
  [@@deriving sexp_of]

  let empty_state =
    let dummy =
      Book.create ~title:"Dummy" ~key:"Key" ~subjects:[] ~isbn:(Some 1)
    in
    let state =
      { visited = []
      ; to_visit = Book.Binary_heap.create ~dummy 1
      ; recommendations = []
      ; current_book = dummy
      }
    in
    state
  ;;
end

(* user said yes to X book, so now we call this on all tis subjects : *)
let update_to_visit_from_subject ~(state : State.t) ~subject =
  let visited = state.visited in
  let to_visit = state.to_visit in
  print_endline "entered update_to_visit";
  let books_raw = Book_fetch.Fetcher.Subjects.fetch_sub subject in
  print_endline "/////////////";
  print_endline (String.prefix books_raw 150);
  let books = Page_parser.Subject_page.parse_books books_raw in
  print_endline "got books from subjects";
  print_s [%sexp (books : Book.t list)];
  List.iter books ~f:(fun (book : Book.t) ->
    let key = Book.key book in
    book.heuristic <- book.heuristic + 1;
    if not
         (List.exists visited ~f:(fun k -> equal 0 (Book.Key.compare k key)))
    then Book.Binary_heap.add to_visit book)
;;

let%expect_test "Get books from subject: Tooth Fairy" =
  let state = State.empty_state in
  update_to_visit_from_subject ~state ~subject:"tooth_fairy";
  Book.Binary_heap.iter (fun book -> Book.print book) state.to_visit
;;

let get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  state.visited <- List.append state.visited [ Book.key new_book ];
  new_book
;;

let%expect_test "Get next book from Tooth Fairy subject original queue" =
  let state = State.empty_state in
  update_to_visit_from_subject ~state ~subject:"tooth_fairy";
  let next_book = get_next_book ~state in
  Book.print next_book
;;

let _calculate_heuristic = ()
(* to implement and call in update_to_visit*)
