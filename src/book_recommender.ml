open! Core
open Async

module State = struct
  type t =
    { mutable visited_books : Book.OL_Id.t list
    ; to_visit : Book.Binary_heap.t
    ; mutable recommendations : Book.t list
    ; mutable current_book : Book.t
    ; mutable visited_subjects : string list
    ; mutable cache : Cache.t
    }
  [@@deriving sexp_of, fields ~getters]

  let empty_state book =
    let dummy =
      Book.create
        ~title:"Dummy"
        ~author:None
        ~ol_id:"Key"
        ~google_id:"Key"
        ~subjects:[]
        ~isbn:(Some 1)
        ~publish_date:None
    in
    let%map new_cache = Cache.create_cache () in
    let state =
      { visited_books = []
      ; to_visit = Book.Binary_heap.create ~dummy 40
      ; recommendations = []
      ; current_book = book
      ; visited_subjects = []
      ; cache = new_cache
      }
    in
    state
  ;;
end

let update_current_book ~(state : State.t) ~new_book =
  state.current_book <- new_book
;;

let update_visited ~(state : State.t) ~(book : Book.t) =
  state.visited_books <- List.append state.visited_books [ book.ol_id ]
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
  if is_in_publish_range origin_book.publish_date instance.publish_date 10
  then
    instance.heuristic
    <- instance.heuristic -. (1.5 -. (distance_from_origin /. 10.0))
  else
    instance.heuristic
    <- instance.heuristic -. (1.0 -. (distance_from_origin /. 10.0))
;;

let update_to_visit_from_subject
  distance_from_origin
  ~(state : State.t)
  ~subject
  =
  let to_visit = state.to_visit in
  if not
       (List.exists state.visited_subjects ~f:(fun s ->
          String.equal s (String.lowercase subject)))
  then (
    print_endline subject;
    state.visited_subjects
    <- List.append state.visited_subjects [ String.lowercase subject ];
    let%bind books =
      Open_library.Fetch_and_parse.get_books_from_subject state.cache subject
    in
    return
      (List.iter books ~f:(fun (book : Book.t) ->
         let key = book.ol_id in
         if not
              (List.exists state.visited_books ~f:(fun visited_key ->
                 equal 0 (Book.OL_Id.compare visited_key key)))
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
           | None -> Book.Binary_heap.add to_visit book))))
  else return ()
;;

let rec get_next_book ~(state : State.t) =
  let new_book = Book.Binary_heap.pop_minimum state.to_visit in
  let language =
    Open_library.Fetch_and_parse.get_first_language_from_title new_book.title
  in
  if String.equal language "eng"
  then (
    update_visited ~state ~book:new_book;
    new_book)
  else get_next_book ~state
;;
