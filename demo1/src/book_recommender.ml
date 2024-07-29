open! Core

module State = struct
  type t =
    { mutable visited_books : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    ; mutable current_book : Book.t
    ; mutable visited_subjects : string list
    }
  [@@deriving sexp_of]

  let empty_state =
    let dummy =
      Book.create
        ~title:"Dummy"
        ~author:None
        ~key:"Key"
        ~subjects:[]
        ~isbn:(Some 1)
    in
    let state =
      { visited_books = []
      ; to_visit = Book.Binary_heap.create ~dummy 1
      ; recommendations = []
      ; current_book = dummy
      ; visited_subjects = []
      }
    in
    state
  ;;
end

(* user said yes to X book, so now we call this on all tis subjects : *)
let update_to_visit_from_subject
  distance_from_origin
  ~(state : State.t)
  ~subject
  =
  let visited_books = state.visited_books in
  let visited_subjects = state.visited_subjects in
  let to_visit = state.to_visit in
  if not
       (List.exists visited_subjects ~f:(fun s ->
          String.equal (String.lowercase s) (String.lowercase subject)))
  then (
    (*print_endline subject;*)
    state.visited_subjects <- List.append state.visited_subjects [ subject ];
    let books_raw =
      Book_fetch.Fetcher.Subjects.fetch_sub subject ~limit:100
    in
    let books = Page_parser.Subject_page.parse_books books_raw in
    List.iter books ~f:(fun (book : Book.t) ->
      let key = book.key in
      if not
           (List.exists visited_books ~f:(fun k ->
              equal 0 (Book.Key.compare k key)))
      then (
        match Hashtbl.find (Book.Binary_heap.index_map to_visit) key with
        | Some index ->
          let array = Book.Binary_heap.data to_visit in
          let instance = Array.get array index in
          instance.heuristic
          <- instance.heuristic -. (1.0 /. distance_from_origin);
          Book.Binary_heap.heapify_after_update_at_index
            instance
            to_visit
            index
        | None -> Book.Binary_heap.add to_visit book)))
;;

let%expect_test "Get books from subject: Fantasy_fiction" =
  let state = State.empty_state in
  update_to_visit_from_subject 1.0 ~state ~subject:"Fantasy_fiction";
  Book.Binary_heap.iter (fun book -> Book.print book) state.to_visit
;;

let get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  state.visited_books
  <- List.append state.visited_books [ Book.key new_book ];
  new_book
;;

let%expect_test "Get next book from Tooth Fairy subject original queue" =
  let state = State.empty_state in
  update_to_visit_from_subject 1.0 ~state ~subject:"tooth_fairy";
  let next_book = get_next_book ~state in
  Book.print next_book
;;

let%expect_test "Remove and Leave Updated at Top" =
  let dummy =
    Book.create ~title:"" ~author:None ~key:"" ~subjects:[] ~isbn:None
  in
  let book_one =
    Book.create ~title:"1" ~author:None ~key:"1" ~subjects:[] ~isbn:None
  in
  book_one.heuristic <- 0.0;
  let book_two =
    Book.create ~title:"2" ~author:None ~key:"2" ~subjects:[] ~isbn:None
  in
  book_two.heuristic <- 2.0;
  let book_three =
    Book.create ~title:"3" ~author:None ~key:"3" ~subjects:[] ~isbn:None
  in
  book_three.heuristic <- 3.0;
  let book_four =
    Book.create ~title:"4" ~author:None ~key:"4" ~subjects:[] ~isbn:None
  in
  book_four.heuristic <- 4.0;
  let book_five =
    Book.create ~title:"5" ~author:None ~key:"5" ~subjects:[] ~isbn:None
  in
  book_five.heuristic <- 5.0;
  let heap = Book.Binary_heap.create ~dummy 3 in
  Book.Binary_heap.add heap book_one;
  Book.Binary_heap.add heap book_two;
  Book.Binary_heap.add heap book_three;
  Book.Binary_heap.add heap book_four;
  Book.Binary_heap.add heap book_five;
  let index = Book.Binary_heap.find_index heap ~key:book_four.key in
  print_s [%message (heap : Book.Binary_heap.t)];
  book_four.heuristic <- 1.0;
  Book.Binary_heap.heapify_after_update_at_index book_four heap index;
  printf "New heap:";
  print_s [%message (heap : Book.Binary_heap.t)]
;;
