open! Core

module State = struct
  type t =
    { mutable visited_books : Book.Key.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    ; mutable current_book : Book.t
    ; mutable visited_subjects : string list
    }
  [@@deriving sexp_of, fields ~getters]

  let empty_state () =
    let dummy =
      Book.create
        ~title:"Dummy"
        ~author:None
        ~key:"Key"
        ~subjects:[]
        ~isbn:(Some 1)
        ~publish_date:None
    in
    let state =
      { visited_books = []
      ; to_visit = Book.Binary_heap.create ~dummy 40
      ; recommendations = []
      ; current_book = dummy
      ; visited_subjects = []
      }
    in
    state
  ;;
end

let update_current_book ~(state : State.t) ~new_book =
  state.current_book <- new_book
;;

let update_visited ~(state : State.t) ~(book : Book.t) =
  state.visited_books <- List.append state.visited_books [ book.key ]
;;

let update_recommendations ~(state : State.t) ~(book : Book.t) =
  state.recommendations <- List.append state.recommendations [ book ]
;;

let is_in_publish_range
  (midpoint : int option)
  (publish_date : int option)
  (range : int)
  : bool
  =
  match midpoint, publish_date with
  | Some mid, Some date -> mid - range <= date && mid + range >= date
  | _, _ -> false
;;

let make_heuristic_change
  (origin_book : Book.t)
  (instance : Book.t)
  (distance_from_origin : float)
  =
  if is_in_publish_range origin_book.publish_date instance.publish_date 5
  then
    instance.heuristic <- instance.heuristic -. (1.5 /. distance_from_origin)
  else
    instance.heuristic <- instance.heuristic -. (1.0 /. distance_from_origin)
;;

let update_to_visit_from_subject
  distance_from_origin
  ~(state : State.t)
  ~subject
  =
  let to_visit = state.to_visit in
  if not
       (List.exists state.visited_subjects ~f:(fun s ->
          String.equal (String.lowercase s) (String.lowercase subject)))
  then (
    print_endline subject;
    state.visited_subjects <- List.append state.visited_subjects [ subject ];
    let books_raw = Google_api.Fetcher.search_by_subject subject in
    let books = Google_api.Parser.get_books_from_subject_search books_raw in
    (* these need to change with new functions that get all the books from
       one subject *)
    List.iter books ~f:(fun (book : Book.t) ->
      let key = book.key in
      if not
           (List.exists state.visited_books ~f:(fun visited_key ->
              equal 0 (Book.Key.compare visited_key key)))
      then (
        match Book.Binary_heap.find_index to_visit ~key with
        | Some index ->
          let array = Book.Binary_heap.data to_visit in
          let instance = Array.get array index in
          make_heuristic_change
            state.current_book
            instance
            distance_from_origin;
          Book.Binary_heap.heapify_after_update_at_index
            instance
            to_visit
            index
        | None -> Book.Binary_heap.add to_visit book)))
;;

let get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  update_visited ~state ~book:new_book;
  new_book
;;
