open! Core

let%expect_test "Get books from subject: Action & Adventure" =
  let state = Book_recommender.State.empty_state () in
  Book_recommender.update_to_visit_from_subject
    1.0
    ~state
    ~subject:"Action & Adventure";
  Book.Binary_heap.iter (fun book -> Book.print book) state.to_visit
;;

(* let%expect_test "Get next book from Tooth Fairy subject original queue" =
   let state = Book_recommender.State.empty_state () in
   Book_recommender.update_to_visit_from_subject 1.0 ~state
   ~subject:"tooth_fairy"; let next_book = Book_recommender.get_next_book
   ~state in print_s [%message (state : Book_recommender.State.t)];
   Book.print next_book ;; *)
let%expect_test "Remove and Updated Heap" =
  let dummy =
    Book.create
      ~title:""
      ~author:None
      ~key:""
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  let book_one =
    Book.create
      ~title:"1"
      ~author:None
      ~key:"1"
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  book_one.heuristic <- 0.0;
  let book_two =
    Book.create
      ~title:"2"
      ~author:None
      ~key:"2"
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  book_two.heuristic <- 2.0;
  let book_three =
    Book.create
      ~title:"3"
      ~author:None
      ~key:"3"
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  book_three.heuristic <- 3.0;
  let book_four =
    Book.create
      ~title:"4"
      ~author:None
      ~key:"4"
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  book_four.heuristic <- 4.0;
  let book_five =
    Book.create
      ~title:"5"
      ~author:None
      ~key:"5"
      ~subjects:[]
      ~isbn:None
      ~publish_date:None
  in
  book_five.heuristic <- 5.0;
  let heap = Book.Binary_heap.create ~dummy 3 in
  Book.Binary_heap.add heap book_one;
  Book.Binary_heap.add heap book_two;
  Book.Binary_heap.add heap book_three;
  Book.Binary_heap.add heap book_four;
  Book.Binary_heap.add heap book_five;
  match Book.Binary_heap.find_index heap ~key:book_four.key with
  | None -> failwith "not found"
  | Some index ->
    book_four.heuristic <- 1.0;
    Book.Binary_heap.heapify_after_update_at_index book_four heap index;
    printf "New heap:";
    print_s [%message (heap : Book.Binary_heap.t)]
;;
